module Main (main) where

import           Development.IDE.Test
import           Ide.Plugin.Notes     (Log, descriptor)
import           System.Directory     (canonicalizePath)
import           System.FilePath      ((</>))
import           Test.Hls

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
        _ <- waitForAllProgressDone
        defs <- getDefinitions doc (Position 3 41)
        liftIO $ do
          fp <- canonicalizePath "NoteDef.hs"
          defs @?= InL (Definition (InL (Location (filePathToUri fp) (Range (Position 5 9) (Position 5 9)))))
    , testCase "no_note" $ runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc "NoteDef.hs" "haskell"
        defs <- getDefinitions doc (Position 1 0)
        liftIO $ defs @?= InL (Definition (InR []))

    , testCase "unopened_file" $ runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc "Other.hs" "haskell"
        waitForCustomMessage "ghcide/cradle/loaded" (const $ Just ())
        waitForAllProgressDone
        defs <- getDefinitions doc (Position 5 20)
        liftIO $ do
          fp <- canonicalizePath "NoteDef.hs"
          defs @?= InL (Definition (InL (Location (filePathToUri fp) (Range (Position 9 6) (Position 9 6)))))
    ]

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-notes-plugin" </> "test" </> "testdata"
