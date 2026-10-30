{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Format (tests) where

import           Control.Lens                ((^.))
import           Control.Monad.IO.Class
import           Data.Functor                (void)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Ide.Types
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           Language.LSP.Test
import           Test.Hls
import           Test.Hls.Command
import           Test.Hls.Flags              (requiresFourmoluPlugin,
                                              requiresOrmoluPlugin)
#if defined(hls_ormolu)
import System.FilePath.Posix ((</>))
import qualified Data.Map as M
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson ((.=))
import qualified Ide.Plugin.Ormolu as Ormolu
#endif

tests :: TestTree
tests = testGroup "format document"
    [ providerTests
    ]

providerTests :: TestTree
providerTests = testGroup "lsp formatting provider"
    [ testCase "respects none" $ runSessionWithConfig (formatConfig "none") hlsLspCommand fullLatestClientCaps "test/testdata/format" $ do
        void configurationRequest
        doc <- openDoc "Format.hs" "haskell"
        resp <- request SMethod_TextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
        liftIO $ case resp ^. L.result of
          result@(Left (TResponseError reason message Nothing)) -> case reason of
            (InR ErrorCodes_MethodNotFound) -> pure () -- No formatter
            (InR ErrorCodes_InvalidRequest) | "No plugin" `T.isPrefixOf` message -> pure ()
            _ -> assertFailure $ "strange response from formatting provider:" ++ show result
          result -> assertFailure $ "strange response from formatting provider:" ++ show result

#if defined(hls_ormolu)
    , testCase "error message contains stderr output" $ do
        let cliConfig = def {
                formattingProvider = "ormolu"
            }
            testDataDir = "plugins" </> "hls-ormolu-plugin" </> "test" </> "testdata"
            ormoluPlugin = mkPluginTestDescriptor Ormolu.descriptor "ormolu"
        runSessionWithServer cliConfig ormoluPlugin testDataDir $ do
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
                         ("ormoluCmd: OrmoluParsingFailed" `T.isInfixOf` msg)
                     -- 2. Contains a stable parse-error phrase from formatter stderr
                     assertBool ("Expected parse error details from stderr, got: " <> T.unpack msg)
                         ("parse error on input" `T.isInfixOf` msg)
                 Right _ ->
                     assertFailure "Expected formatting to fail on unparsable file"
#endif

    , requiresOrmoluPlugin . requiresFourmoluPlugin $ testCase "can change on the fly" $ runSessionWithConfig (formatConfig "none") hlsLspCommand fullLatestClientCaps "test/testdata/format" $ do
        void configurationRequest

        doc <- openDoc "Format.hs" "haskell"

        setHlsConfig (formatLspConfig "ormolu")
        formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
        formattedOrmolu <- liftIO $ T.readFile "test/testdata/format/Format.ormolu.formatted.hs"
        documentContents doc >>= liftIO . (@?= formattedOrmolu)

        setHlsConfig (formatLspConfig "fourmolu")
        formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
        formattedFourmolu <- liftIO $ T.readFile "test/testdata/format/Format.fourmolu.formatted.hs"
        documentContents doc >>= liftIO . (@?= formattedFourmolu)

        setHlsConfig (formatLspConfig "ormolu")
        formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
        formattedOrmoluPostFourmolu <- liftIO $ T.readFile "test/testdata/format/Format.ormolu_post_fourmolu.formatted.hs"
        documentContents doc >>= liftIO . (@?= formattedOrmoluPostFourmolu)
    ]

formatLspConfig :: T.Text -> Config
formatLspConfig provider = def { formattingProvider = provider }

formatConfig :: T.Text -> SessionConfig
formatConfig provider = defaultConfig { lspConfig = hlsConfigToClientConfig (formatLspConfig provider), ignoreConfigurationRequests = False }
