{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Control.Lens                ((^.))
import           Data.Aeson
import qualified Data.Aeson.KeyMap           as KM
import           Data.Functor
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Ide.Plugin.Config
import qualified Ide.Plugin.Fourmolu         as Fourmolu
import qualified Language.LSP.Protocol.Lens  as L
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
        , testCase "error message contains stderr output" $ do
            let cliConfig = def {
                    formattingProvider = "fourmolu",
                    plugins = M.fromList [("fourmolu", def { plcConfig = KM.fromList ["external" .= True] })]
                }
            runSessionWithServer cliConfig fourmoluPlugin testDataDir $ do
                doc <- openDoc "FormatError.hs" "haskell"
                void waitForBuildQueue
                resp <- request SMethod_TextDocumentFormatting $
                    DocumentFormattingParams Nothing doc (FormattingOptions 4 True Nothing Nothing Nothing)
                liftIO $ case resp ^. L.result of
                    Left err -> do
                        let msg = err ^. L.message
                        -- Verify the error message structure:
                        -- 1. Contains the exit code prefix (base message intact)
                        assertBool ("Expected exit code prefix, got: " <> T.unpack msg)
                            ("failed with exit code" `T.isInfixOf` msg)
                        -- 2. Contains stderr parse error details
                        assertBool ("Expected parse error details from stderr, got: " <> T.unpack msg)
                            ("parse error" `T.isInfixOf` msg)
                    Right _ ->
                        assertFailure "Expected formatting to fail on invalid syntax file"
        ]

goldenWithFourmolu :: Bool -> TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithFourmolu cli title path desc = goldenWithHaskellDocFormatter def fourmoluPlugin "fourmolu" conf title testDataDir path desc "hs"
 where
  conf = def{plcConfig = KM.fromList ["external" .= cli]}

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-fourmolu-plugin" </> "test" </> "testdata"
