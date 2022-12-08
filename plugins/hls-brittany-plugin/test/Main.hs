{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.Brittany as Brittany
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

brittanyPlugin :: PluginTestDescriptor ()
brittanyPlugin = mkPluginTestDescriptor' Brittany.descriptor "brittany"

tests :: TestTree
tests = testGroup "brittany"
  [ brittanyGolden "formats a document with LF endings" "BrittanyLF" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)

  , brittanyGolden "formats a document with CRLF endings" "BrittanyCRLF" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)

  , brittanyGolden "formats a range with LF endings" "BrittanyLF" "formatted_range" $ \doc -> do
      let range = Range (Position 1 0) (Position 2 22)
      formatRange doc (FormattingOptions 4 True Nothing Nothing Nothing) range

  , brittanyGolden "formats a range with CRLF endings" "BrittanyCRLF" "formatted_range" $ \doc -> do
      let range = Range (Position 1 0) (Position 2 22)
      formatRange doc (FormattingOptions 4 True Nothing Nothing Nothing) range
  ]

brittanyGolden :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
brittanyGolden title path desc = goldenWithHaskellDocFormatter brittanyPlugin "brittany" def title testDataDir path desc "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
