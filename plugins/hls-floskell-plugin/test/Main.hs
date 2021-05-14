{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Ide.Plugin.Floskell     as Floskell
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

plugin :: PluginDescriptor IdeState
plugin = Floskell.descriptor "floskell"

tests :: TestTree
tests = testGroup "floskell"
  [ goldenGitDiff "formats a document" "test/testdata/Floskell.formatted_document.hs" $ runSessionWithServerFormatter plugin "floskell" "test/testdata" $ do
        doc <- openDoc "Floskell.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        TL.encodeUtf8 . TL.fromStrict <$> documentContents doc

  , goldenGitDiff "formats a range" "test/testdata/Floskell.formatted_range.hs" $ runSessionWithServerFormatter plugin "floskell" "test/testdata" $ do
      doc <- openDoc "Floskell.hs" "haskell"
      let range = Range (Position 1 0) (Position 4 22)
      formatRange doc (FormattingOptions 4 True Nothing Nothing Nothing) range
      TL.encodeUtf8 . TL.fromStrict <$> documentContents doc
  ]
