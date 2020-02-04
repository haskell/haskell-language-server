module HieInstall where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Extra                      ( unlessM
                                                          , mapMaybeM
                                                          )
import           Data.Maybe                               ( isJust )
import           System.Directory                         ( listDirectory )
import           System.Environment                       ( unsetEnv )
import           System.Info                              ( os
                                                          , arch
                                                          )

import           Data.Maybe                               ( isNothing
                                                          , mapMaybe
                                                          )
import           Data.List                                ( dropWhileEnd
                                                          , intersperse
                                                          , intercalate
                                                          , sort
                                                          , sortOn
                                                          )
import qualified Data.Text                     as T
import           Data.Char                                ( isSpace )
import           Data.Version                             ( parseVersion
                                                          , makeVersion
                                                          , showVersion
                                                          )
import           Data.Function                            ( (&) )
import           Text.ParserCombinators.ReadP             ( readP_to_S )

import           BuildSystem
import           Stack
import           Cabal
import           Version
import           Print
import           Env
import           Help

defaultMain :: IO ()
defaultMain = do
  -- unset GHC_PACKAGE_PATH for cabal
  unsetEnv "GHC_PACKAGE_PATH"

  -- used for cabal-based targets
  ghcPaths <- findInstalledGhcs
  let cabalVersions = map fst ghcPaths

  -- used for stack-based targets
  stackVersions <- getHieVersions

  let versions = if isRunFromStack then stackVersions else cabalVersions

  let toolsVersions = BuildableVersions stackVersions cabalVersions

  let latestVersion = last versions

  shakeArgs shakeOptions { shakeFiles = "_build" } $ do
    want ["short-help"]
    -- general purpose targets
    phony "submodules"  updateSubmodules
    phony "short-help"  shortHelpMessage
    phony "help"        (helpMessage toolsVersions)

    phony "check" (if isRunFromStack then checkStack else checkCabal_)

    phony "data" $ do
      need ["submodules"]
      need ["check"]
      if isRunFromStack then stackBuildData else cabalBuildData

    forM_
      versions
      (\version -> phony ("haskell-language-server-" ++ version) $ do
        need ["submodules"]
        need ["check"]
        if isRunFromStack then do
          stackInstallHieWithErrMsg (Just version)
        else
          cabalInstallHie version
      )

    phony "latest" (need ["haskell-language-server-" ++ latestVersion])
    phony "haskell-language-server"  (need ["data", "latest"])

    -- stack specific targets
    when isRunFromStack $
      phony "dev" $ stackInstallHieWithErrMsg Nothing

    -- cabal specific targets
    when isRunFromCabal $ do
      -- It throws an error if there is no ghc in $PATH
      checkInstalledGhcs ghcPaths
      phony "ghcs" $ showInstalledGhcs ghcPaths

    -- macos specific targets
    phony "icu-macos-fix"
          (need ["icu-macos-fix-install"] >> need ["icu-macos-fix-build"])
    phony "icu-macos-fix-install" (command_ [] "brew" ["install", "icu4c"])
    phony "icu-macos-fix-build" $ mapM_ buildIcuMacosFix versions


buildIcuMacosFix :: VersionNumber -> Action ()
buildIcuMacosFix version = execStackWithGhc_
  version
  [ "build"
  , "text-icu"
  , "--extra-lib-dirs=/usr/local/opt/icu4c/lib"
  , "--extra-include-dirs=/usr/local/opt/icu4c/include"
  ]

-- | update the submodules that the project is in the state as required by the `stack.yaml` files
updateSubmodules :: Action ()
updateSubmodules = do
  command_ [] "git" ["submodule", "sync"]
  command_ [] "git" ["submodule", "update", "--init"]
