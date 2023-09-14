{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Data.Aeson
import           Data.Functor
import qualified Data.Text                   as T
import           Ide.Plugin.Config
import qualified Ide.Plugin.Ormolu           as Ormolu
import           Language.LSP.Protocol.Types
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

ormoluPlugin :: PluginTestDescriptor Ormolu.LogEvent
ormoluPlugin = mkPluginTestDescriptor Ormolu.descriptor "ormolu"

tests :: TestTree
tests = testGroup "ormolu" $
    [False, True] <&> \cli ->
    testGroup (if cli then "cli" else "lib")
      [ goldenWithOrmolu cli "formats correctly" "Ormolu" "formatted" $ \doc -> do
          formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
      , goldenWithOrmolu cli "formats imports correctly" "Ormolu2" "formatted" $ \doc -> do
          formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
#if MIN_VERSION_ormolu(0,5,3)
      , goldenWithOrmolu cli "formats operators correctly" "Ormolu3" "formatted" $ \doc -> do
          formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
#endif
      ]

goldenWithOrmolu :: Bool -> TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithOrmolu cli title path desc = goldenWithHaskellDocFormatter def ormoluPlugin "ormolu" def title testDataDir path desc "hs"
 where
  conf = def{plcConfig = (\(Object obj) -> obj) $ object ["external" .= cli]}

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
