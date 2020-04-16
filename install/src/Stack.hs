{-# LANGUAGE CPP #-}
module Stack where

import           Control.Monad
import           Control.Exception
import           System.FilePath
import           System.Directory                         ( copyFile )
import           System.Process
import           Version
import           Print
import           Utils

stackInstallHlsWithErrMsg :: Maybe VersionNumber -> [String] -> IO ()
stackInstallHlsWithErrMsg mbVersionNumber args =
  stackInstallHls mbVersionNumber args
    `onException` (putStrLn stackBuildFailMsg)

-- | copy the built binaries into the localBinDir
stackInstallHls :: Maybe VersionNumber -> [String] -> IO ()
stackInstallHls mbVersionNumber args = do
  versionNumber <-
    case mbVersionNumber of
      Nothing -> do
        execStackWithCfgFile_ "stack.yaml" $ 
          ["install"
          , "haskell-language-server-wrapper"
          , "haskell-language-server"] ++ args
        getGhcVersionOfCfgFile "stack.yaml" args
      Just vn -> do
        execStackWithGhc_ vn $ ["install"] ++ args
        return vn

  localBinDir <- getLocalBin args
  let hls = "haskell-language-server" <.> exe
  
  copyFile (localBinDir </> hls)
            (localBinDir </> "haskell-language-server-" ++ versionNumber <.> exe)
  copyFile (localBinDir </> hls)
            (localBinDir </> "haskell-language-server-" ++ dropExtension versionNumber <.> exe)

getGhcVersionOfCfgFile :: String -> [String] -> IO VersionNumber
getGhcVersionOfCfgFile stackFile args = do
  ghcVersion <-
    execStackWithCfgFile stackFile $ ["exec", "ghc"] ++ args ++ ["--", "--numeric-version"]
  return $ trim ghcVersion

-- | check `stack` has the required version
checkStack :: [String] -> IO ()
checkStack args = do
  stackVersion <- trimmedStdout <$> (execStackShake $ ["--numeric-version"] ++ args)
  unless (checkVersion requiredStackVersion stackVersion) $ do
    printInStars $ stackExeIsOldFailMsg stackVersion
    error $ stackExeIsOldFailMsg stackVersion

-- | Get the local binary path of stack.
-- Equal to the command `stack path --local-bin`
getLocalBin :: [String] -> IO FilePath
getLocalBin args = do
  stackLocalDir' <- execStackShake $ ["path", "--local-bin"] ++ args
  return $ trim stackLocalDir'

stackBuildData :: [String] -> IO ()
stackBuildData args = do
  execStackShake_ $ ["build", "hoogle"] ++ args
  execStackShake_ $ ["exec", "hoogle", "generate"] ++ args

-- | Execute a stack command for a specified ghc, discarding the output
execStackWithGhc_ :: VersionNumber -> [String] -> IO ()
execStackWithGhc_ =  fmap void . execStackWithGhc

-- | Execute a stack command for a specified ghc
execStackWithGhc :: VersionNumber -> [String] -> IO String
execStackWithGhc versionNumber args = do
  let stackFile = "stack-" ++ versionNumber ++ ".yaml"
  execStackWithCfgFile stackFile args

-- | Execute a stack command for a specified stack.yaml file, discarding the output
execStackWithCfgFile_ :: String -> [String] -> IO ()
execStackWithCfgFile_ = fmap void . execStackWithCfgFile

-- | Execute a stack command for a specified stack.yaml file
execStackWithCfgFile :: String -> [String] -> IO String
execStackWithCfgFile stackFile args =
  readProcess "stack" (("--stack-yaml=" ++ stackFile) : args) ""

-- | Execute a stack command with the same resolver as the build script
execStackShake :: [String] -> IO String
execStackShake args = 
  readProcess "stack" ("--stack-yaml=install/shake.yaml" : args) ""

-- | Execute a stack command with the same resolver as the build script, discarding the output
execStackShake_ :: [String] -> IO ()
execStackShake_ = fmap void execStackShake


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
    ++ "\thttps://github.com/haskell/haskell-language-engine"
