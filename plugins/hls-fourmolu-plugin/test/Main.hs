{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap           as KM
import           Data.Functor
import           Ide.Plugin.Config
import qualified Ide.Plugin.Fourmolu         as Fourmolu
import           Language.LSP.Protocol.Types
import           Language.LSP.Test
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

fourmoluPlugin :: PluginTestDescriptor Fourmolu.LogEvent
fourmoluPlugin = mkPluginTestDescriptor Fourmolu.descriptor "fourmolu"

tests :: TestTree
tests =
  testGroup "fourmolu" $
    [False, True] <&> \cli ->
      testGroup
        (if cli then "cli" else "lib")
        [ goldenWithFourmolu cli "formats correctly" "Fourmolu" "formatted" $ \doc -> do
            formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        , goldenWithFourmolu cli "formats imports correctly" "Fourmolu2" "formatted" $ \doc -> do
            formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        , goldenWithFourmolu cli "uses correct operator fixities" "Fourmolu3" "formatted" $ \doc -> do
            formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        ]

goldenWithFourmolu :: Bool -> TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithFourmolu cli title path desc = goldenWithHaskellDocFormatter def fourmoluPlugin "fourmolu" conf title testDataDir path desc "hs"
 where
  conf = def{plcConfig = KM.fromList ["external" .= cli]}

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-fourmolu-plugin" </> "test" </> "testdata"
