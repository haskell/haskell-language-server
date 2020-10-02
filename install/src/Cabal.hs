{-# LANGUAGE CPP #-}
module Cabal where

import           Development.Shake
import           Development.Shake.FilePath
import           Control.Monad
import           System.Directory                         ( copyFile )
import           System.Info                              ( os )

import           Version
import           Print
import           Env
#if RUN_FROM_STACK
import           Control.Exception                        ( throwIO )
#else
import           Cabal.Config
import           Data.Functor.Identity
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

cabalBuildData :: [String] -> Action ()
cabalBuildData args = do
  execCabal_ $ ["v2-build", "hoogle"] ++ args
  execCabal_ $ ["v2-exec", "hoogle", "generate"] ++ args

getGhcPathOfOrThrowError :: VersionNumber -> Action GhcPath
getGhcPathOfOrThrowError versionNumber =
  getGhcPathOf versionNumber >>= \case
    Nothing -> do
      printInStars $ ghcVersionNotFoundFailMsg versionNumber
      error (ghcVersionNotFoundFailMsg versionNumber)
    Just p -> return p

cabalInstallHls :: VersionNumber -> [String] -> Action ()
cabalInstallHls versionNumber args = do
  localBin <- liftIO $ getInstallDir
  cabalVersion <- getCabalVersion args
  ghcPath <- getGhcPathOfOrThrowError versionNumber

  let isCabal3 = checkVersion [3,0,0,0] cabalVersion
      installDirOpt | isCabal3 = "--installdir"
                    | otherwise = "--symlink-bindir"
      installMethod | isWindowsSystem && isCabal3 = ["--install-method=copy"]
                    | otherwise = []

  projectFile <- getProjectFile versionNumber

  execCabal_ $
    [ "v2-install"
    , "exe:haskell-language-server"
    , "exe:haskell-language-server-wrapper"
    , "-w", ghcPath
    , "--write-ghc-environment-files=never"
    , installDirOpt, localBin
    , "--max-backjumps=5000"
    , "--overwrite-policy=always"
    , "--project-file=" ++ projectFile
    ]
    ++ installMethod
    ++ args

  let minorVerExe = "haskell-language-server-" ++ versionNumber <.> exe
      majorVerExe = "haskell-language-server-" ++ dropExtension versionNumber <.> exe

  let copyCmd old new = if os == "mingw32"
                        then liftIO $ copyFile old new
                        else command [] "ln" ["-f", old, new]
  copyCmd (localBin </> "haskell-language-server" <.> exe) (localBin </> minorVerExe)
  copyCmd (localBin </> "haskell-language-server" <.> exe) (localBin </> majorVerExe)

  printLine $   "Copied executables "
             ++ ("haskell-language-server-wrapper" <.> exe) ++ ", "
             ++ ("haskell-language-server" <.> exe) ++ ", "
             ++ majorVerExe ++ " and "
             ++ minorVerExe
             ++ " to " ++ localBin

getProjectFile :: VersionNumber -> Action FilePath
getProjectFile ver = do
  existFile <- doesFileExist $ "cabal.project-" ++ ver
  return $ if existFile
            then "cabal.project-" ++ ver
            else "cabal.project"

checkCabal_ :: [String] -> Action ()
checkCabal_ args = checkCabal args >> return ()

-- | check `cabal` has the required version
checkCabal :: [String] -> Action String
checkCabal args = do
  cabalVersion <- getCabalVersion args
  unless (checkVersion requiredCabalVersion cabalVersion) $ do
    printInStars $ cabalInstallIsOldFailMsg cabalVersion
    error $ cabalInstallIsOldFailMsg cabalVersion
  return cabalVersion

getCabalVersion :: [String] -> Action String
getCabalVersion args = trimmedStdout <$> (execCabal $ ["--numeric-version"] ++ args)

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

getVerbosityArg :: Verbosity -> String
getVerbosityArg v = "-v" ++ cabalVerbosity
  where cabalVerbosity = case v of
          Silent ->     "0"
#if MIN_VERSION_shake(0,18,4)
          Error ->      "0"
          Warn ->       "1"
          Info ->       "1"
          Verbose ->    "2"
#else
          Quiet ->      "0"
          Normal ->     "1"
          Loud ->       "2"
          Chatty ->     "2"
#endif
          Diagnostic -> "3"
