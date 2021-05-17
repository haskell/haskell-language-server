{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.Fourmolu as Fourmolu
import           Language.LSP.Test
import           Language.LSP.Types
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

fourmoluPlugin :: PluginDescriptor IdeState
fourmoluPlugin = Fourmolu.descriptor "fourmolu"

tests :: TestTree
tests = testGroup "fourmolu"
  [ goldenWithFourmolu "formats correctly" "Fourmolu" "formatted" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
  , goldenWithFourmolu "formats imports correctly" "Fourmolu" "formatted" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
  ]

goldenWithFourmolu :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithFourmolu title path desc = goldenWithHaskellDocFormatter fourmoluPlugin "fourmolu" title testDataDir path desc "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
