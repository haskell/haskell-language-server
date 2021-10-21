module Env where

import           Control.Monad
import           Control.Monad.Extra        (mapMaybeM)
import           Control.Monad.IO.Class
import           Data.Function              (on, (&))
import           Data.List                  (isInfixOf, sort, sortBy)
import           Data.List.Extra            (nubOrdBy, trim)
import           Data.Maybe                 (isJust, mapMaybe)
import           Data.Ord                   (comparing)
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory           (findExecutable, findExecutables,
                                             listDirectory)
import           System.Info                (os)

import qualified Data.Text                  as T

import           Print
import           Version


type GhcPath = String

existsExecutable :: MonadIO m => String -> m Bool
existsExecutable executable = liftIO $ isJust <$> findExecutable executable


-- | Check if the current system is windows
isWindowsSystem :: Bool
isWindowsSystem = os `elem` ["mingw32", "win32"]

findInstalledGhcs :: IO [(VersionNumber, GhcPath)]
findInstalledGhcs = do
  hlsVersions <- getHlsVersions :: IO [VersionNumber]
  knownGhcs <- mapMaybeM
    (\version -> getGhcPathOf version >>= \case
        Nothing -> return Nothing
        Just p  -> return $ Just (version, p)
    )
    (reverse hlsVersions)
  -- filter out not supported ghc versions
  availableGhcs <- filter ((`elem` hlsVersions) . fst) <$> getGhcPaths
  return
    -- sort by version to make it coherent with getHlsVersions
    $ sortBy (comparing fst)
    -- nub by version. knownGhcs takes precedence.
    $ nubOrdBy (compare `on` fst)
    -- filter out stack provided GHCs (assuming that stack programs path is the default one in linux)
    $ filter (not . isInfixOf ".stack" . snd) (knownGhcs ++ availableGhcs)

showInstalledGhcs :: MonadIO m => [(VersionNumber, GhcPath)] -> m ()
showInstalledGhcs ghcPaths = do
  let msg = "Found the following *supported by HLS* GHC paths: \n"
              ++ unlines
                  (map (\(version, path) -> "ghc-" ++ version ++ ": " ++ path)
                    ghcPaths
                  )
  printInStars msg

checkInstalledGhcs :: MonadIO m => [(VersionNumber, GhcPath)] -> m ()
checkInstalledGhcs ghcPaths = when (null ghcPaths) $ do
  let msg = "No ghc installations found in $PATH. \n"
             ++ "The script requires at least one ghc in $PATH \n"
             ++ "  to be able to build haskell-language-server.\n"
  printInStars msg
  error msg

-- | Get the path to a GHC that has the version specified by `VersionNumber`
-- If no such GHC can be found, Nothing is returned.
-- First, it is checked whether there is a GHC with the name `ghc-$VersionNumber`.
-- If this yields no result, it is checked, whether the numeric-version of the `ghc`
-- command fits to the desired version.
getGhcPathOf :: MonadIO m => VersionNumber -> m (Maybe GhcPath)
getGhcPathOf ghcVersion =
  liftIO $ findExecutable ("ghc-" ++ ghcVersion <.> exe) >>= \case
    Nothing -> lookup ghcVersion <$> getGhcPaths
    path    -> return path

-- | Get a list of GHCs that are available in $PATH
getGhcPaths :: MonadIO m => m [(VersionNumber, GhcPath)]
getGhcPaths = liftIO $ do
  paths <- findExecutables "ghc"
  forM paths $ \path -> do
    Stdout version <- cmd path ["--numeric-version"]
    return (trim version, path)

-- | No suitable ghc version has been found. Show a message.
ghcVersionNotFoundFailMsg :: VersionNumber -> String
ghcVersionNotFoundFailMsg versionNumber =
  "No GHC with version "
    <> versionNumber
    <> " has been found.\n"
    <> "Either install a fitting GHC, use the stack targets or modify the PATH variable accordingly."


-- | Defines all different hls versions that are buildable.
--
-- The current directory is scanned for `stack-*.yaml` files.
getHlsVersions :: MonadIO m => m [VersionNumber]
getHlsVersions = do
  let stackYamlPrefix = T.pack "stack-"
  let stackYamlSuffix = T.pack ".yaml"
  files <- liftIO $ listDirectory "."
  let hlsVersions =
        files
          & map T.pack
          & mapMaybe
              (T.stripPrefix stackYamlPrefix >=> T.stripSuffix stackYamlSuffix)
          & map T.unpack
          & sort
  return hlsVersions


-- | Most recent version of hls.
-- Shown in the more concise help message.
mostRecentHlsVersion :: MonadIO m => m VersionNumber
mostRecentHlsVersion = last <$> getHlsVersions
