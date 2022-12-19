{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.Ormolu  as Ormolu
import           Language.LSP.Types
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

ormoluPlugin :: PluginTestDescriptor ()
ormoluPlugin = mkPluginTestDescriptor' Ormolu.descriptor "ormolu"

tests :: TestTree
tests = testGroup "ormolu"
  [ goldenWithOrmolu "formats correctly" "Ormolu" "formatted" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
  , goldenWithOrmolu "formats imports correctly" "Ormolu2" "formatted" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
  ]

goldenWithOrmolu :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithOrmolu title path desc = goldenWithHaskellDocFormatter ormoluPlugin "ormolu" def title testDataDir path desc "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
