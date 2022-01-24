{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Lens               hiding (List, (<.>))
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSChar8
import           Data.String                (fromString)
import           Ide.Plugin.SelectionRange  (descriptor)
import           Language.LSP.Types.Lens
import           System.FilePath            ((<.>), (</>))
import           Test.Hls

plugin :: PluginDescriptor IdeState
plugin = descriptor "selectionRange"

main :: IO ()
main = defaultTestRunner $
    testGroup "Selection Range"
        [ goldenTest "Import" [(4, 36), (1, 8)]
        , goldenTest "Function" [(5, 19), (5, 12), (4, 4), (3, 5)]
        ]

-- | build a golden test for
goldenTest :: TestName -> [(UInt, UInt)] -> TestTree
goldenTest testName positions = goldenGitDiff testName (testDataDir </> testName <.> "golden" <.> "txt") $ do
    res <- runSessionWithServer plugin testDataDir $ do
        doc <- openDoc (testName <.> "hs") "haskell"
        resp <- request STextDocumentSelectionRange $ SelectionRangeParams Nothing Nothing doc
            (List $ fmap (uncurry Position . (\(x, y) -> (x-1, y-1))) positions)
        let res = resp ^. result
        pure $ fmap showSelectionRangesForTest res
    case res of
        Left err     -> assertFailure (show err)
        Right golden -> pure golden

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

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
