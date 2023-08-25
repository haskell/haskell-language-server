{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Data.Text                   as T
import qualified Ide.Plugin.Ormolu           as Ormolu
import           Language.LSP.Protocol.Types
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

ormoluPlugin :: PluginTestDescriptor T.Text
ormoluPlugin = mkPluginTestDescriptor Ormolu.descriptor "ormolu"

tests :: TestTree
tests = testGroup "ormolu"
  [ goldenWithOrmolu "formats correctly" "Ormolu" "formatted" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
  , goldenWithOrmolu "formats imports correctly" "Ormolu2" "formatted" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
#if MIN_VERSION_ormolu(0,5,3)
  , goldenWithOrmolu "formats operators correctly" "Ormolu3" "formatted" $ \doc -> do
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
#endif
  ]

goldenWithOrmolu :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithOrmolu title path desc = goldenWithHaskellDocFormatter def ormoluPlugin "ormolu" def title testDataDir path desc "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
