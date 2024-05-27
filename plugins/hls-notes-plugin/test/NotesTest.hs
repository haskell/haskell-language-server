module Main (main) where

import           Ide.Plugin.Notes (Log, descriptor)
import           System.FilePath  ((</>))
import           Test.Hls

plugin :: PluginTestDescriptor Log
plugin = mkPluginTestDescriptor descriptor "notes"

main :: IO ()
main = defaultTestRunner $
  testGroup "Notes"
    [ gotoNoteTests
    ]

runSessionWithServer' :: FilePath -> (FilePath -> Session a) -> IO a
runSessionWithServer' fp act =
    runSessionWithTestConfig def
        { testLspConfig = def
        , testPluginDescriptor = plugin
        , testDirLocation = Left fp
        } act

gotoNoteTests :: TestTree
gotoNoteTests = testGroup "Goto Note Definition"
    [
      testCase "single_file" $ runSessionWithServer' testDataDir $ \dir -> do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 3 41)
        let fp = dir </> "NoteDef.hs"
        liftIO $ defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 8 9) (Position 8 9))]))
    , testCase "liberal_format" $ runSessionWithServer' testDataDir $ \dir -> do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 5 64)
        let fp = dir </> "NoteDef.hs"
        liftIO $ defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 18 11) (Position 18 11))]))

    , testCase "invalid_note" $ runSessionWithServer' testDataDir $ const $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 6 54)
        liftIO $ defs @?= InL (Definition (InR []))

    , testCase "no_note" $ runSessionWithServer' testDataDir $ const $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 1 0)
        liftIO $ defs @?= InL (Definition (InR []))

    , testCase "unopened_file" $ runSessionWithServer' testDataDir $ \dir -> do
        doc <- openDoc "Other.hs" "haskell"
        waitForKickDone
        defs <- getDefinitions doc (Position 5 20)
        let fp = dir </> "NoteDef.hs"
        liftIO $ defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 12 6) (Position 12 6))]))
    ]

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-notes-plugin" </> "test" </> "testdata"
