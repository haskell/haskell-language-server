{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.CabalFmt as CabalFmt
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

cabalFmtPlugin :: PluginDescriptor IdeState
cabalFmtPlugin = CabalFmt.descriptor mempty "cabal-fmt"

tests :: TestTree
tests = testGroup "cabal-fmt"
  [ cabalFmtGolden "formats a simple document" "simple_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)

  , cabalFmtGolden "formats a document with expand:src comment" "commented_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)

  , cabalFmtGolden "formats a document with lib information" "lib_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 10 True Nothing Nothing Nothing)
  ]

cabalFmtGolden :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
cabalFmtGolden title path desc = goldenWithCabalDocFormatter cabalFmtPlugin "cabal-fmt" conf title testDataDir path desc "cabal"
  where
    conf = def

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
