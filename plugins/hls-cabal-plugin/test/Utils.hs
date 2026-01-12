{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Utils where

import           Control.Monad                     (guard)
import           Data.List                         (sort)
import           Data.Proxy                        (Proxy (Proxy))
import qualified Data.Text                         as T
import           Ide.Plugin.Cabal                  (descriptor,
                                                    haskellInteractionDescriptor)
import qualified Ide.Plugin.Cabal
import           Ide.Plugin.Cabal.Completion.Types
import           System.FilePath
import           Test.Hls
import qualified Test.Hls.FileSystem               as FS


cabalPlugin :: PluginTestDescriptor Ide.Plugin.Cabal.Log
cabalPlugin = mkPluginTestDescriptor descriptor "cabal"

cabalHaskellPlugin :: PluginTestDescriptor Ide.Plugin.Cabal.Log
cabalHaskellPlugin = mkPluginTestDescriptor haskellInteractionDescriptor "cabal-haskell"

simpleCabalPrefixInfoFromPos :: Position -> T.Text -> CabalPrefixInfo
simpleCabalPrefixInfoFromPos pos prefix =
    CabalPrefixInfo
        { completionPrefix = prefix
        , completionCursorPosition = pos
        , isStringNotation = Nothing
        , completionRange = Range pos (Position 0 0)
        , completionWorkingDir = ""
        , completionFileName = "test"
        }

simpleCabalPrefixInfoFromFp :: T.Text -> FilePath -> CabalPrefixInfo
simpleCabalPrefixInfoFromFp prefix fp =
    CabalPrefixInfo
        { completionPrefix = prefix
        , isStringNotation = Nothing
        , completionCursorPosition = Position 0 0
        , completionRange = Range (Position 0 0) (Position 0 0)
        , completionWorkingDir = fp
        , completionFileName = "test"
        }

filePathComplTestDir :: FilePath
filePathComplTestDir = addTrailingPathSeparator $ testDataDir </> "filepath-completions"

runCabalTestCaseSession :: TestName -> FilePath -> Session () -> TestTree
runCabalTestCaseSession title subdir = testCase title . runCabalSession subdir

runHaskellTestCaseSession :: TestName -> FilePath -> Session () -> TestTree
runHaskellTestCaseSession title subdir = testCase title . runHaskellAndCabalSession (FS.mkVirtualFileTree testDataDir [FS.copyDir subdir])

runCabalSession :: FilePath -> Session a -> IO a
runCabalSession subdir =
    failIfSessionTimeout . runSessionWithServerInTmpDir def cabalPlugin (FS.mkVirtualFileTree testDataDir [FS.copyDir subdir])

runCabalTestCaseSessionVft :: TestName -> FS.VirtualFileTree -> Session () -> TestTree
runCabalTestCaseSessionVft title vft = testCase title . runCabalSessionVft vft

runCabalSessionVft :: FS.VirtualFileTree -> Session a -> IO a
runCabalSessionVft vft =
    failIfSessionTimeout . runSessionWithServerInTmpDir def cabalPlugin vft

runHaskellAndCabalSession :: FS.VirtualFileTree -> Session a -> IO a
runHaskellAndCabalSession vft =
    failIfSessionTimeout . runSessionWithServerInTmpDir def (cabalPlugin <> cabalHaskellPlugin) vft

runCabalGoldenSession :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
runCabalGoldenSession title subdir fp act = goldenWithCabalDoc def cabalPlugin title testDataDir (subdir </> fp) "golden" "cabal" act

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-cabal-plugin" </> "test" </> "testdata"

-- | these functions are used to detect cabal kicks
-- and look at diagnostics for cabal files
-- kicks are run everytime there is a shake session run/restart
cabalKickDone :: Session ()
cabalKickDone = kick (Proxy @"kick/done/cabal") >>= guard . not . null

cabalKickStart :: Session ()
cabalKickStart = kick (Proxy @"kick/start/cabal") >>= guard . not . null

cabalCaptureKick :: Session [Diagnostic]
cabalCaptureKick = captureKickDiagnostics cabalKickStart cabalKickDone

-- | list comparison where the order in the list is irrelevant
(@?==) :: (HasCallStack, Ord a, Show a) => [a] -> [a] -> Assertion
(@?==) l1 l2 = sort l1 @?= sort l2

