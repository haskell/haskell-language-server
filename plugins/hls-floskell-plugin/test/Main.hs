{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.Floskell as Floskell
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

floskellPlugin :: PluginTestDescriptor ()
floskellPlugin = mkPluginTestDescriptor' Floskell.descriptor "floskell"

tests :: TestTree
tests = testGroup "floskell"
  [ goldenWithFloskell "formats a document" "Floskell" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)

  , goldenWithFloskell "formats a range" "Floskell" "formatted_range" $ \doc -> do
      let range = Range (Position 1 0) (Position 4 22)
      formatRange doc (FormattingOptions 4 True Nothing Nothing Nothing) range
  ]

goldenWithFloskell :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithFloskell title path desc = goldenWithHaskellDocFormatter floskellPlugin "floskell" def title testDataDir path desc "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
