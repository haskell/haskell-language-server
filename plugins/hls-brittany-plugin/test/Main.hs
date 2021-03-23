{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Test.Hls

main :: IO ()
main = defaultTestRunner tests

tests :: TestTree
tests = testGroup "brittany" [
    goldenGitDiff "formats a document with LF endings" "test/testdata/BrittanyLF.formatted_document.hs" $ runSession testCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyLF.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenGitDiff "formats a document with CRLF endings" "test/testdata/BrittanyCRLF.formatted_document.hs" $ runSession testCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyCRLF.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenGitDiff "formats a range with LF endings" "test/testdata/BrittanyLF.formatted_range.hs" $ runSession testCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyLF.hs" "haskell"
        let range = Range (Position 1 0) (Position 2 22)
        formatRange doc (FormattingOptions 4 True Nothing Nothing Nothing) range
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenGitDiff "formats a range with CRLF endings" "test/testdata/BrittanyCRLF.formatted_range.hs" $ runSession testCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyCRLF.hs" "haskell"
        let range = Range (Position 1 0) (Position 2 22)
        formatRange doc (FormattingOptions 4 True Nothing Nothing Nothing) range
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    ]
