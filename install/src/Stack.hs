module Stack where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Control.Exception
import           Control.Monad
import           Data.List
import           System.Directory                         ( copyFile )
import           System.FilePath                          ( splitSearchPath, searchPathSeparator, (</>) )
import           System.Environment                       ( lookupEnv, setEnv, getEnvironment )
import           System.IO.Error                          ( isDoesNotExistError )
import           BuildSystem
import           Version
import           Print
import           Env

stackInstallHieWithErrMsg :: Maybe VersionNumber -> Action ()
stackInstallHieWithErrMsg mbVersionNumber =
  stackInstallHie mbVersionNumber
    `actionOnException` liftIO (putStrLn stackBuildFailMsg)

-- | copy the built binaries into the localBinDir
stackInstallHie :: Maybe VersionNumber -> Action ()
stackInstallHie mbVersionNumber = do
  versionNumber <-
    case mbVersionNumber of
      Nothing -> do
        execStackWithCfgFile_ "stack.yaml" ["install", "haskell-language-server"]
        getGhcVersionOfCfgFile "stack.yaml"
      Just vn -> do
        execStackWithGhc_ vn ["install", "haskell-language-server"]
        return vn

  localBinDir <- getLocalBin
  let hie = "haskell-language-server" <.> exe
  liftIO $ do
    copyFile (localBinDir </> hie)
             (localBinDir </> "haskell-language-server-" ++ versionNumber <.> exe)
    copyFile (localBinDir </> hie)
             (localBinDir </> "haskell-language-server-" ++ dropExtension versionNumber <.> exe)

getGhcVersionOfCfgFile :: String -> Action VersionNumber
getGhcVersionOfCfgFile stackFile = do
  Stdout ghcVersion <-
    execStackWithCfgFile stackFile ["exec", "ghc", "--", "--numeric-version"]
  return $ trim ghcVersion

-- | check `stack` has the required version
checkStack :: Action ()
checkStack = do
  stackVersion <- trimmedStdout <$> execStackShake ["--numeric-version"]
  unless (checkVersion requiredStackVersion stackVersion) $ do
    printInStars $ stackExeIsOldFailMsg stackVersion
    error $ stackExeIsOldFailMsg stackVersion

-- | Get the local binary path of stack.
-- Equal to the command `stack path --local-bin`
getLocalBin :: Action FilePath
getLocalBin = do
  Stdout stackLocalDir' <- execStackShake ["path", "--local-bin"]
  return $ trim stackLocalDir'

stackBuildData :: Action ()
stackBuildData = do
  execStackShake_ ["build", "hoogle"]
  execStackShake_ ["exec", "hoogle", "generate"]

-- | Execute a stack command for a specified ghc, discarding the output
execStackWithGhc_ :: VersionNumber -> [String] -> Action ()
execStackWithGhc_ = execStackWithGhc

-- | Execute a stack command for a specified ghc
execStackWithGhc :: CmdResult r => VersionNumber -> [String] -> Action r
execStackWithGhc versionNumber args = do
  let stackFile = "stack-" ++ versionNumber ++ ".yaml"
  execStackWithCfgFile stackFile args

-- | Execute a stack command for a specified stack.yaml file, discarding the output
execStackWithCfgFile_ :: String -> [String] -> Action ()
execStackWithCfgFile_ = execStackWithCfgFile

-- | Execute a stack command for a specified stack.yaml file
execStackWithCfgFile :: CmdResult r => String -> [String] -> Action r
execStackWithCfgFile stackFile args =
  command [] "stack" (("--stack-yaml=" ++ stackFile) : args)

-- | Execute a stack command with the same resolver as the build script
execStackShake :: CmdResult r => [String] -> Action r
execStackShake args = command [] "stack" ("--stack-yaml=install/shake.yaml" : args)

-- | Execute a stack command with the same resolver as the build script, discarding the output
execStackShake_ :: [String] -> Action ()
execStackShake_ = execStackShake


-- | Error message when the `stack` binary is an older version
stackExeIsOldFailMsg :: String -> String
stackExeIsOldFailMsg stackVersion =
  "The `stack` executable is outdated.\n"
    ++ "found version is `"
    ++ stackVersion
    ++ "`.\n"
    ++ "required version is `"
    ++ versionToString requiredStackVersion
    ++ "`.\n"
    ++ "Please run `stack upgrade` to upgrade your stack installation"

requiredStackVersion :: RequiredVersion
requiredStackVersion = [2, 1, 1]

-- |Stack build fails message
stackBuildFailMsg :: String
stackBuildFailMsg =
  embedInStars
    $  "Building failed, "
    ++ "Try running `stack clean` and restart the build\n"
    ++ "If this does not work, open an issue at \n"
    ++ "\thttps://github.com/haskell/haskell-language-server-engine"
