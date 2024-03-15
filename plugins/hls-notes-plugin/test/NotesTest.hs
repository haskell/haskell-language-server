module Main (main) where

import           Development.IDE.Test
import           Ide.Plugin.Notes     (Log, descriptor)
import           System.Directory     (canonicalizePath)
import           System.FilePath      ((</>))
import           Test.Hls             hiding (waitForBuildQueue)

plugin :: PluginTestDescriptor Log
plugin = mkPluginTestDescriptor descriptor "notes"

main :: IO ()
main = defaultTestRunner $
  testGroup "Notes"
    [ gotoNoteTests
    ]

gotoNoteTests :: TestTree
gotoNoteTests = testGroup "Goto Note Definition"
    [ testCase "single_file" $ runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForBuildQueue
        waitForAllProgressDone
        defs <- getDefinitions doc (Position 3 41)
        liftIO $ do
          fp <- canonicalizePath "NoteDef.hs"
          defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 8 9) (Position 8 9))]))
    , testCase "liberal_format" $ runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForBuildQueue
        waitForAllProgressDone
        defs <- getDefinitions doc (Position 5 64)
        liftIO $ do
          fp <- canonicalizePath "NoteDef.hs"
          defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 18 11) (Position 18 11))]))

    , testCase "invalid_note" $ runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForBuildQueue
        waitForAllProgressDone
        defs <- getDefinitions doc (Position 6 54)
        liftIO $ do
            defs @?= InL (Definition (InR []))

    , testCase "no_note" $ runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        waitForBuildQueue
        waitForAllProgressDone
        defs <- getDefinitions doc (Position 1 0)
        liftIO $ defs @?= InL (Definition (InR []))

    , testCase "unopened_file" $ runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc "Other.hs" "haskell"
        waitForCustomMessage "ghcide/cradle/loaded" (const $ Just ())
        waitForBuildQueue
        waitForAllProgressDone
        defs <- getDefinitions doc (Position 5 20)
        liftIO $ do
          fp <- canonicalizePath "NoteDef.hs"
          defs @?= InL (Definition (InR [Location (filePathToUri fp) (Range (Position 12 6) (Position 12 6))]))
    ]

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-notes-plugin" </> "test" </> "testdata"
