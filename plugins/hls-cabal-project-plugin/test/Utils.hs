{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Ide.Plugin.CabalProject
import           System.FilePath         ((</>))
import           Test.Hls
import qualified Test.Hls.FileSystem     as FS

pluginProject :: PluginTestDescriptor Ide.Plugin.CabalProject.Log
pluginProject = mkPluginTestDescriptor Ide.Plugin.CabalProject.descriptor "cabal-project"

runProjectSession :: FS.VirtualFileTree -> Session a -> IO a
runProjectSession = runSessionWithServerInTmpDir def pluginProject

runProjectTestCaseSession :: TestName -> FilePath -> Session () -> TestTree
runProjectTestCaseSession title subdir =
  testCase title . runProjectSession (FS.mkVirtualFileTree testDataDir [FS.copyDir subdir])

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-cabal-project-plugin" </> "test" </> "testdata"
