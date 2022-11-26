{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Data.Aeson
import           Data.Functor
import           Ide.Plugin.Config
import qualified Ide.Plugin.Fourmolu as Fourmolu
import           Language.LSP.Test
import           Language.LSP.Types
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
        , goldenWithFourmolu cli "formats imports correctly" "Fourmolu" "formatted" $ \doc -> do
            formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        ]

goldenWithFourmolu :: Bool -> TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithFourmolu cli title path desc = goldenWithHaskellDocFormatter fourmoluPlugin "fourmolu" conf title testDataDir path desc "hs"
 where
  conf = def{plcConfig = (\(Object obj) -> obj) $ object ["external" .= cli]}

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
