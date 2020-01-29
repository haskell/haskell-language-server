module Env where

import           Development.Shake
import           Development.Shake.Command
import           Control.Monad.IO.Class
import           Control.Monad
import           Development.Shake.FilePath
import           System.Info                              ( os
                                                          , arch
                                                          )
import           Data.Maybe                               ( isJust
                                                          , isNothing
                                                          , mapMaybe
                                                          )
import           System.Directory                         ( findExecutable
                                                          , findExecutables
                                                          , listDirectory
                                                          )
import           Data.Function                            ( (&)
                                                          , on
                                                          )
import           Data.List                                ( sort
                                                          , sortBy
                                                          , isInfixOf
                                                          , nubBy
                                                          )
import           Data.Ord                                 ( comparing )
import           Control.Monad.Extra                      ( mapMaybeM )

import qualified Data.Text                     as T

import           Version
import           Print


type GhcPath = String

existsExecutable :: MonadIO m => String -> m Bool
existsExecutable executable = liftIO $ isJust <$> findExecutable executable


-- | Check if the current system is windows
isWindowsSystem :: Bool
isWindowsSystem = os `elem` ["mingw32", "win32"]

findInstalledGhcs :: IO [(VersionNumber, GhcPath)]
findInstalledGhcs = do
  hieVersions <- getHieVersions :: IO [VersionNumber]
  knownGhcs <- mapMaybeM
    (\version -> getGhcPathOf version >>= \case
        Nothing -> return Nothing
        Just p  -> return $ Just (version, p)
    )
    (reverse hieVersions)
  -- filter out not supported ghc versions
  availableGhcs <- filter ((`elem` hieVersions) . fst) <$> getGhcPaths
  return
    -- sort by version to make it coherent with getHieVersions
    $ sortBy (comparing fst)
    -- nub by version. knownGhcs takes precedence.
    $ nubBy ((==) `on` fst)
    -- filter out stack provided GHCs (assuming that stack programs path is the default one in linux)
    $ filter (not . isInfixOf ".stack" . snd) (knownGhcs ++ availableGhcs)

-- | Get the path to a GHC that has the version specified by `VersionNumber`
-- If no such GHC can be found, Nothing is returned.
-- First, it is checked whether there is a GHC with the name `ghc-$VersionNumber`.
-- If this yields no result, it is checked, whether the numeric-version of the `ghc`
-- command fits to the desired version.
getGhcPathOf :: MonadIO m => VersionNumber -> m (Maybe GhcPath)
getGhcPathOf ghcVersion =
  liftIO $ findExecutable ("ghc-" ++ ghcVersion <.> exe) >>= \case
    Nothing -> lookup ghcVersion <$> getGhcPaths
    path -> return path

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


-- | Defines all different hie versions that are buildable.
--
-- The current directory is scanned for `stack-*.yaml` files.
getHieVersions :: MonadIO m => m [VersionNumber]
getHieVersions = do
  let stackYamlPrefix = T.pack "stack-"
  let stackYamlSuffix = T.pack ".yaml"
  files <- liftIO $ listDirectory "."
  let hieVersions =
        files
          & map T.pack
          & mapMaybe
              (T.stripPrefix stackYamlPrefix >=> T.stripSuffix stackYamlSuffix)
          & map T.unpack
        -- the following line excludes `8.6.3`, `8.8.1` and `8.8.2` on windows systems
          & filter (\p -> not (isWindowsSystem && p `elem` ["8.6.3", "8.8.1", "8.8.2"]))
          & sort
  return hieVersions


-- | Most recent version of hie.
-- Shown in the more concise help message.
mostRecentHieVersion :: MonadIO m => m VersionNumber
mostRecentHieVersion = last <$> getHieVersions
