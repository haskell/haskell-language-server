{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Lens                   hiding (List, (<.>))
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy.Char8     as LBSChar8
import           Data.String                    (fromString)
import           Development.IDE.Types.Logger   (Priority (Debug),
                                                 Recorder (Recorder),
                                                 WithPriority (WithPriority),
                                                 makeDefaultStderrRecorder,
                                                 pretty)
import           Ide.Plugin.CodeRange           (Log, descriptor)
import qualified Ide.Plugin.CodeRange.RulesTest
import qualified Ide.Plugin.CodeRangeTest
import           Language.LSP.Types.Lens
import           System.FilePath                ((<.>), (</>))
import           Test.Hls

plugin :: Recorder (WithPriority Log) -> PluginDescriptor IdeState
plugin recorder = descriptor recorder "codeRange"

main :: IO ()
main = do
    recorder <- contramap (fmap pretty) <$> makeDefaultStderrRecorder Nothing Debug
    defaultTestRunner $
        testGroup "Code Range" [
            testGroup "Integration Tests" [
                makeSelectionRangeGoldenTest recorder "Import" [(4, 36), (1, 8)],
                makeSelectionRangeGoldenTest recorder "Function" [(5, 19), (5, 12), (4, 4), (3, 5)]
            ],
            testGroup "Unit Tests" [
                -- Ide.Plugin.CodeRangeTest.testTree,
                Ide.Plugin.CodeRange.RulesTest.testTree
            ]
        ]

makeSelectionRangeGoldenTest :: Recorder (WithPriority Log) -> TestName -> [(UInt, UInt)] -> TestTree
makeSelectionRangeGoldenTest recorder testName positions = goldenGitDiff testName (testDataDir </> testName <.> "golden" <.> "txt") $ do
    res <- runSessionWithServer (plugin recorder) testDataDir $ do
        doc <- openDoc (testName <.> "hs") "haskell"
        resp <- request STextDocumentSelectionRange $ SelectionRangeParams Nothing Nothing doc
            (List $ fmap (uncurry Position . (\(x, y) -> (x-1, y-1))) positions)
        let res = resp ^. result
        pure $ fmap showSelectionRangesForTest res
    case res of
        Left err     -> assertFailure (show err)
        Right golden -> pure golden
  where
    testDataDir :: FilePath
    testDataDir = "test" </> "testdata" </> "selection-range"

    showSelectionRangesForTest :: List SelectionRange -> ByteString
    showSelectionRangesForTest (List selectionRanges) = LBSChar8.intercalate "\n" $ fmap showSelectionRangeForTest selectionRanges

    showSelectionRangeForTest :: SelectionRange -> ByteString
    showSelectionRangeForTest selectionRange = go True (Just selectionRange)
      where
        go :: Bool -> Maybe SelectionRange -> ByteString
        go _ Nothing = ""
        go isFirst (Just (SelectionRange (Range sp ep) parent)) =
            (if isFirst then "" else " => ") <> showPosition sp <> " " <> showPosition ep <> go False parent
        showPosition :: Position -> ByteString
        showPosition (Position line col) = "(" <> showLBS (line + 1) <> "," <> showLBS (col + 1) <> ")"
        showLBS = fromString . show
