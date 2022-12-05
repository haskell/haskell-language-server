{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.StylishHaskell as StylishHaskell
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

stylishHaskellPlugin :: PluginTestDescriptor ()
stylishHaskellPlugin = mkPluginTestDescriptor' StylishHaskell.descriptor "stylishHaskell"

tests :: TestTree
tests = testGroup "stylish-haskell"
  [ goldenWithStylishHaskell "formats a document" "StylishHaskell" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
  , goldenWithStylishHaskell "formats a range" "StylishHaskell" "formatted_range" $ \doc -> do
      formatRange doc (FormattingOptions 2 True Nothing Nothing Nothing) (Range (Position 0 0) (Position 2 21))
  ]

goldenWithStylishHaskell :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithStylishHaskell title fp desc = goldenWithHaskellDocFormatter stylishHaskellPlugin "stylishHaskell" def title testDataDir fp desc "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
