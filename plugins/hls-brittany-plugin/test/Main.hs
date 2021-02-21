{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Language.LSP.Test
import Language.LSP.Types
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.Runners (
    consoleTestReporter,
    listingTests,
 )
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Runners.AntXML

main :: IO ()
main = defaultMainWithIngredients
        [antXMLRunner, rerunningTests [listingTests, consoleTestReporter]]
        tests

testCommand = "test-server"

tests :: TestTree
tests = testGroup "brittany" [
    goldenVsStringDiff "formats a document with LF endings" goldenGitDiff "test/testdata/BrittanyLF.formatted_document.hs" $ runSession testCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyLF.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenVsStringDiff "formats a document with CRLF endings" goldenGitDiff "test/testdata/BrittanyCRLF.formatted_document.hs" $ runSession testCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyCRLF.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenVsStringDiff "formats a range with LF endings" goldenGitDiff "test/testdata/BrittanyLF.formatted_range.hs" $ runSession testCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyLF.hs" "haskell"
        let range = Range (Position 1 0) (Position 2 22)
        formatRange doc (FormattingOptions 4 True Nothing Nothing Nothing) range
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenVsStringDiff "formats a range with CRLF endings" goldenGitDiff "test/testdata/BrittanyCRLF.formatted_range.hs" $ runSession testCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyCRLF.hs" "haskell"
        let range = Range (Position 1 0) (Position 2 22)
        formatRange doc (FormattingOptions 4 True Nothing Nothing Nothing) range
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    ]

goldenGitDiff :: FilePath -> FilePath -> [String]
goldenGitDiff fRef fNew = ["git", "diff", "--no-index", "--text", "--exit-code", fRef, fNew]
