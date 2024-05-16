{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap           as KM
import           Data.Functor
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
goldenWithOrmolu cli title path desc =
  goldenWithHaskellDocFormatter def ormoluPlugin "ormolu" conf title testDataDir path desc "hs"
 where
  conf = def{plcConfig = KM.fromList ["external" .= cli]}

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-ormolu-plugin" </> "test" </> "testdata"
