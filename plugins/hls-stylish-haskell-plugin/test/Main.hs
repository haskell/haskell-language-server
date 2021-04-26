{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Data.ByteString.Lazy      as BS
import qualified Data.Text.Encoding        as T
import qualified Data.Text.IO              as T
import qualified Ide.Plugin.StylishHaskell as StylishHaskell
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

plugin :: PluginDescriptor IdeState
plugin = StylishHaskell.descriptor "stylishHaskell"

tests :: TestTree
tests = testGroup "stylish-haskell" [
  goldenGitDiff "formats a document" "test/testdata/StylishHaskell.formatted_document.hs" $ runSessionWithServerFormatter plugin "stylishHaskell" "test/testdata" $ do
      doc <- openDoc "StylishHaskell.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenGitDiff "formats a range" "test/testdata/StylishHaskell.formatted_range.hs" $ runSessionWithServerFormatter plugin "stylishHaskell" "test/testdata" $ do
      doc <- openDoc "StylishHaskell.hs" "haskell"
      formatRange doc (FormattingOptions 2 True Nothing Nothing Nothing) (Range (Position 0 0) (Position 2 21))
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  ]
