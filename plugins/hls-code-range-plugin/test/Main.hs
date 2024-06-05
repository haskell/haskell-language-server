{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Lens                   hiding (List, (<.>))
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy.Char8     as LBSChar8
import           Data.String                    (fromString)
import           Ide.Plugin.CodeRange           (Log, descriptor)
import qualified Ide.Plugin.CodeRange.RulesTest
import qualified Ide.Plugin.CodeRangeTest
import           Language.LSP.Protocol.Lens     (result)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           System.FilePath                ((<.>), (</>))
import           Test.Hls

plugin :: PluginTestDescriptor Log
plugin = mkPluginTestDescriptor descriptor "codeRange"

main :: IO ()
main = do
    defaultTestRunner $
        testGroup "Code Range" [
            testGroup "Integration Tests" [
                selectionRangeGoldenTest "Import" [(4, 36), (1, 8)],
                selectionRangeGoldenTest "Function" [(5, 19), (5, 12), (4, 4), (3, 5)],
                selectionRangeGoldenTest "Empty" [(1, 5)],
                foldingRangeGoldenTest "Function"
            ],
            testGroup "Unit Tests" [
                Ide.Plugin.CodeRangeTest.testTree,
                Ide.Plugin.CodeRange.RulesTest.testTree
            ]
        ]

selectionRangeGoldenTest :: TestName -> [(UInt, UInt)] -> TestTree
selectionRangeGoldenTest testName positions = goldenGitDiff testName (testDataDir </> testName <.> "golden" <.> "txt") $ do
    res <- runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc (testName <.> "hs") "haskell"
        resp <- request SMethod_TextDocumentSelectionRange $ SelectionRangeParams Nothing Nothing doc
            $ fmap (uncurry Position . (\(x, y) -> (x-1, y-1))) positions
        let res = resp ^. result
        pure $ fmap (showSelectionRangesForTest . absorbNull) res
    case res of
        Left (TResponseError (InL LSPErrorCodes_RequestFailed) _ _) -> pure ""
        Left err     -> assertFailure (show err)
        Right golden -> pure golden
  where
    testDataDir :: FilePath
    testDataDir = "plugins" </> "hls-code-range-plugin" </> "test" </> "testdata" </> "selection-range"

    showSelectionRangesForTest :: [SelectionRange] -> ByteString
    showSelectionRangesForTest selectionRanges = LBSChar8.intercalate "\n" $ fmap showSelectionRangeForTest selectionRanges

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

foldingRangeGoldenTest :: TestName -> TestTree
foldingRangeGoldenTest testName = goldenGitDiff  testName (testDataDir </> testName <.> "golden" <.> "txt") $ do
    res <- runSessionWithServer def plugin testDataDir $ do
        doc <- openDoc (testName <.> "hs") "haskell"
        resp <- request SMethod_TextDocumentFoldingRange $ FoldingRangeParams Nothing Nothing doc
        let res = resp ^. result
        pure $ fmap (showFoldingRangesForTest . absorbNull) res

    case res of
        Left err     -> assertFailure (show err)
        Right golden -> pure golden

    where
        testDataDir :: FilePath
        testDataDir = "plugins" </> "hls-code-range-plugin" </> "test" </> "testdata" </> "folding-range"

        showFoldingRangesForTest :: [FoldingRange] -> ByteString
        showFoldingRangesForTest foldingRanges = (LBSChar8.intercalate "\n" $ fmap showFoldingRangeForTest foldingRanges) `LBSChar8.snoc` '\n'

        showFoldingRangeForTest :: FoldingRange -> ByteString
        showFoldingRangeForTest (FoldingRange sl (Just sc) el (Just ec) (Just frk) _) =
            "((" <> showLBS sl <> ", " <> showLBS sc <> ") : (" <> showLBS el <> ", " <> showLBS ec <> ")) : " <> showFRK frk
        showFoldingRangeForTest fr =
            "unexpected FoldingRange: " <> fromString (show fr)

        showLBS = fromString . show
        showFRK = fromString . show
