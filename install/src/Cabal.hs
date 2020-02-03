{-# LANGUAGE CPP #-}

module Cabal where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Control.Monad
import           Data.Maybe                               ( isNothing
                                                          , isJust
                                                          )
import           Control.Monad.Extra                      ( whenMaybe )
import           System.Directory                         ( findExecutable
                                                          , copyFile
                                                          )

import           Version
import           Print
import           Env
import           Data.Functor.Identity
#if RUN_FROM_STACK
import           Control.Exception                        ( throwIO )
#else
import           Cabal.Config
#endif

getInstallDir :: IO FilePath
#if RUN_FROM_STACK
-- we should never hit this codepath
getInstallDir = throwIO $ userError "Stack and cabal should never be mixed"
#else
getInstallDir = runIdentity . cfgInstallDir <$> readConfig
#endif

execCabal :: CmdResult r => [String] -> Action r
execCabal = command [] "cabal"

execCabal_ :: [String] -> Action ()
execCabal_ = execCabal

cabalBuildData :: Action ()
cabalBuildData = do
  execCabal_ ["v2-build", "hoogle"]
  execCabal_ ["v2-exec", "hoogle", "generate"]

getGhcPathOfOrThrowError :: VersionNumber -> Action GhcPath
getGhcPathOfOrThrowError versionNumber =
  getGhcPathOf versionNumber >>= \case
    Nothing -> do
      printInStars $ ghcVersionNotFoundFailMsg versionNumber
      error (ghcVersionNotFoundFailMsg versionNumber)
    Just p -> return p

cabalInstallHie :: VersionNumber -> Action ()
cabalInstallHie versionNumber = do
  localBin <- liftIO $ getInstallDir
  cabalVersion <- getCabalVersion
  ghcPath <- getGhcPathOfOrThrowError versionNumber

  let isCabal3 = checkVersion [3,0,0,0] cabalVersion
      installDirOpt | isCabal3 = "--installdir"
                    | otherwise = "--symlink-bindir"
      installMethod | isWindowsSystem && isCabal3 = ["--install-method=copy"]
                    | otherwise = []
  execCabal_ $
    [ "v2-install"
    , "exe:haskell-language-server"
    , "exe:haskell-language-server-wrapper"
    , "-w", ghcPath
    , "--write-ghc-environment-files=never"
    , installDirOpt, localBin
    , "--max-backjumps=5000"
    , "--overwrite-policy=always"
    ]
    ++ installMethod

  let minorVerExe = "haskell-language-server-" ++ versionNumber <.> exe
      majorVerExe = "haskell-language-server-" ++ dropExtension versionNumber <.> exe

  liftIO $ do
    copyFile (localBin </> "haskell-language-server" <.> exe) (localBin </> minorVerExe)
    copyFile (localBin </> "haskell-language-server" <.> exe) (localBin </> majorVerExe)

  printLine $   "Copied executables "
             ++ ("haskell-language-server-wrapper" <.> exe) ++ ", "
             ++ ("haskell-language-server" <.> exe) ++ ", "
             ++ majorVerExe ++ " and "
             ++ minorVerExe
             ++ " to " ++ localBin

checkCabal_ :: Action ()
checkCabal_ = checkCabal >> return ()

-- | check `cabal` has the required version
checkCabal :: Action String
checkCabal = do
  cabalVersion <- getCabalVersion
  unless (checkVersion requiredCabalVersion cabalVersion) $ do
    printInStars $ cabalInstallIsOldFailMsg cabalVersion
    error $ cabalInstallIsOldFailMsg cabalVersion
  return cabalVersion

getCabalVersion :: Action String
getCabalVersion = trimmedStdout <$> execCabal ["--numeric-version"]

-- | Error message when the `cabal` binary is an older version
cabalInstallIsOldFailMsg :: String -> String
cabalInstallIsOldFailMsg cabalVersion =
  "The `cabal` executable found in $PATH is outdated.\n"
    ++ "found version is `"
    ++ cabalVersion
    ++ "`.\n"
    ++ "required version is `"
    ++ versionToString requiredCabalVersion
    ++ "`."


requiredCabalVersion :: RequiredVersion
requiredCabalVersion | isWindowsSystem = requiredCabalVersionForWindows
                     | otherwise = [2, 4, 1, 0]

requiredCabalVersionForWindows :: RequiredVersion
requiredCabalVersionForWindows = [3, 0, 0, 0]
