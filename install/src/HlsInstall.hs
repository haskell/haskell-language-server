module HlsInstall where

import           Control.Monad
import           Development.Shake
import           System.Environment (unsetEnv)

import           BuildSystem
import           Cabal
import           Env
import           Help
import           Stack
import           Version

defaultMain :: IO ()
defaultMain = do
  -- unset GHC_PACKAGE_PATH for cabal
  unsetEnv "GHC_PACKAGE_PATH"

  -- used for cabal-based targets
  ghcPaths <- findInstalledGhcs
  let cabalVersions = map fst ghcPaths

  -- used for stack-based targets
  stackVersions <- getHlsVersions

  let versions = if isRunFromStack then stackVersions else cabalVersions

  let toolsVersions = BuildableVersions stackVersions cabalVersions

  let latestVersion = last versions

  shakeArgs shakeOptions { shakeFiles = "_build" } $ do

    shakeOptionsRules <- getShakeOptionsRules

    let verbosityArg = if isRunFromStack then Stack.getVerbosityArg else Cabal.getVerbosityArg

    let args = [verbosityArg (shakeVerbosity shakeOptionsRules)]

    phony "show-options" $ do
      putNormal "Options:"
      putNormal $ "    Verbosity level: " ++ show (shakeVerbosity shakeOptionsRules)

    want ["short-help"]
    -- general purpose targets
    phony "short-help"  shortHelpMessage
    phony "help"        (helpMessage toolsVersions)

    phony "check" (if isRunFromStack then checkStack args else checkCabal_ args)

    phony "data" $ do
      need ["show-options"]
      need ["check"]
      liftIO $ putStrLn "Generation of hoogle data files is disabled for now."
      -- if isRunFromStack then stackBuildData args else cabalBuildData args

    forM_
      versions
      (\version -> phony ("hls-" ++ version) $ do
        need ["show-options"]
        need ["check"]
        if isRunFromStack then
          stackInstallHlsWithErrMsg (Just version) args
        else
          cabalInstallHls version args
      )

    unless (null versions) $ do
      phony "latest" (need ["hls-" ++ latestVersion])
      phony "hls"  (need ["data", "latest"])

    -- stack specific targets
    -- Default `stack.yaml` uses ghc-8.8.2 and we can't build hls in windows
    -- TODO: Enable for windows when it uses ghc-8.8.3
    when isRunFromStack $
      phony "dev" $ do
        need ["show-options"]
        stackInstallHlsWithErrMsg Nothing args

    -- cabal specific targets
    when isRunFromCabal $ do
      -- It throws an error if there is no ghc in $PATH
      checkInstalledGhcs ghcPaths
      phony "ghcs" $ showInstalledGhcs ghcPaths

    -- macos specific targets
    phony "icu-macos-fix" $ do
      need ["show-options"]
      need ["icu-macos-fix-install"]
      need ["icu-macos-fix-build"]

    phony "icu-macos-fix-install" (command_ [] "brew" ["install", "icu4c"])
    phony "icu-macos-fix-build" $ mapM_ (flip buildIcuMacosFix args) versions


buildIcuMacosFix :: VersionNumber -> [String] -> Action ()
buildIcuMacosFix version args = execStackWithGhc_
  version $
  [ "build"
  , "text-icu"
  , "--extra-lib-dirs=/usr/local/opt/icu4c/lib"
  , "--extra-include-dirs=/usr/local/opt/icu4c/include"
  ] ++ args
