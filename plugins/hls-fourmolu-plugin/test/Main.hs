{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy         as BS
import qualified Data.Text.Encoding           as T
import qualified Data.Text.IO                 as T
import           Language.LSP.Test
import           Language.LSP.Types
import           Test.Hls
import qualified Ide.Plugin.Fourmolu  as Fourmolu
import           System.FilePath

main :: IO ()
main = defaultTestRunner tests

plugin :: PluginDescriptor IdeState
plugin = Fourmolu.descriptor "fourmolu"

testDataDir :: FilePath
testDataDir = "test/testdata/"

tests :: TestTree
tests = testGroup "fourmolu"
  [ goldenGitDiff "formats correctly" (testDataDir </> "Format.fourmolu.formatted.hs") $
    runSessionWithServerFormatter plugin "fourmolu" testDataDir $ do
        doc <- openDoc "Format.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenGitDiff "formats imports correctly" (testDataDir </> "Format2.fourmolu.formatted.hs") $
    runSessionWithServerFormatter plugin "fourmolu" testDataDir $ do
        doc <- openDoc "Format2.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  ]
