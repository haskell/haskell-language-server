{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Utils where

import           Control.Monad                 (guard)
import           Data.List                     (sort)
import           Data.Proxy                    (Proxy (Proxy))
import qualified Data.Text                     as T
import           Ide.Plugin.CabalProject       (descriptor)
import qualified Ide.Plugin.CabalProject
import           Ide.Plugin.CabalProject.Types
import           System.FilePath
import           Test.Hls


cabalProjectPlugin :: PluginTestDescriptor Ide.Plugin.CabalProject.Log
cabalProjectPlugin = mkPluginTestDescriptor descriptor "cabal-project"

runCabalProjectTestCaseSession :: TestName -> FilePath -> Session () -> TestTree
runCabalProjectTestCaseSession title subdir = testCase title . runCabalProjectSession subdir

runCabalProjectSession :: FilePath -> Session a -> IO a
runCabalProjectSession subdir =
    failIfSessionTimeout . runSessionWithServer def cabalProjectPlugin (testDataDir </> subdir)

runCabalProjectGoldenSession :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
runCabalProjectGoldenSession title subdir fp act = goldenWithCabalDoc def cabalProjectPlugin title testDataDir (subdir </> fp) "golden" "cabal-project" act

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-cabal-project-plugin" </> "test" </> "testdata"

-- | these functions are used to detect cabal.project kicks
-- and look at diagnostics for cabal.project files
-- kicks are run everytime there is a shake session run/restart
cabalProjectKickDone :: Session ()
cabalProjectKickDone = kick (Proxy @"kick/done/cabal-project") >>= guard . not . null

cabalProjectKickStart :: Session ()
cabalProjectKickStart = kick (Proxy @"kick/start/cabal-project") >>= guard . not . null

cabalProjectCaptureKick :: Session [Diagnostic]
cabalProjectCaptureKick = captureKickDiagnostics cabalProjectKickStart cabalProjectKickDone
