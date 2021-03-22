{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy         as BS
import qualified Data.Text.Encoding           as T
import qualified Data.Text.IO                 as T
import           Language.LSP.Test
import           Language.LSP.Types
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners           (consoleTestReporter,
                                               listingTests)
import           Test.Tasty.Runners.AntXML

main :: IO ()
main = defaultMainWithIngredients
        [antXMLRunner, rerunningTests [listingTests, consoleTestReporter]]
        tests

testCommand = "test-server"

tests :: TestTree
tests = testGroup "stylish-haskell" [
  goldenVsStringDiff "formats a document" goldenGitDiff "test/testdata/StylishHaskell.formatted_document.hs" $ runSession testCommand fullCaps "test/testdata" $ do
      doc <- openDoc "StylishHaskell.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenVsStringDiff "formats a range" goldenGitDiff "test/testdata/StylishHaskell.formatted_range.hs" $ runSession testCommand fullCaps "test/testdata" $ do
      doc <- openDoc "StylishHaskell.hs" "haskell"
      formatRange doc (FormattingOptions 2 True Nothing Nothing Nothing) (Range (Position 0 0) (Position 2 21))
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  ]

goldenGitDiff :: FilePath -> FilePath -> [String]
goldenGitDiff fRef fNew = ["git", "diff", "--no-index", "--text", "--exit-code", fRef, fNew]
