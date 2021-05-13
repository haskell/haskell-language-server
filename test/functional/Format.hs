{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Format (tests) where

import           Control.Lens            ((^.))
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Text.Encoding      as T
import qualified Data.Text.IO            as T
import           Language.LSP.Test
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens as LSP
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup "format document" [
    goldenGitDiff "works" "test/testdata/format/Format.formatted_document.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , goldenGitDiff "works with custom tab size" "test/testdata/format/Format.formatted_document_with_tabsize.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatDoc doc (FormattingOptions 5 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , rangeTests
    , providerTests
    , ormoluTests
    , fourmoluTests
    ]

rangeTests :: TestTree
rangeTests = testGroup "format range" [
    goldenGitDiff "works" "test/testdata/format/Format.formatted_range.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatRange doc (FormattingOptions 2 True Nothing Nothing Nothing) (Range (Position 5 0) (Position 7 10))
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , goldenGitDiff "works with custom tab size" "test/testdata/format/Format.formatted_range_with_tabsize.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatRange doc (FormattingOptions 5 True Nothing Nothing Nothing) (Range (Position 8 0) (Position 11 19))
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    ]

providerTests :: TestTree
providerTests = testGroup "formatting provider" [
    testCase "respects none" $ runSessionWithConfig (formatConfig "none") hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        resp <- request STextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
        liftIO $ resp ^. LSP.result @?= Left (ResponseError InvalidRequest "No plugin enabled for STextDocumentFormatting, available: []" Nothing)

    , testCase "can change on the fly" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        formattedOrmolu <- liftIO $ T.readFile "test/testdata/format/Format.ormolu.formatted.hs"
        formattedFloskell <- liftIO $ T.readFile "test/testdata/format/Format.floskell.formatted.hs"
        formattedOrmoluPostFloskell <- liftIO $ T.readFile "test/testdata/format/Format.ormolu_post_floskell.formatted.hs"

        doc <- openDoc "Format.hs" "haskell"

        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
        formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
        documentContents doc >>= liftIO . (@?= formattedOrmolu)

        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "floskell"))
        formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
        documentContents doc >>= liftIO . (@?= formattedFloskell)

        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
        formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
        documentContents doc >>= liftIO . (@?= formattedOrmoluPostFloskell)
    , testCase "supports both new and old configuration sections" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
       formattedOrmolu <- liftIO $ T.readFile "test/testdata/format/Format.ormolu.formatted.hs"
       formattedFloskell <- liftIO $ T.readFile "test/testdata/format/Format.floskell.formatted.hs"

       doc <- openDoc "Format.hs" "haskell"

       sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfigOld "ormolu"))
       formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
       documentContents doc >>= liftIO . (@?= formattedOrmolu)

       sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfigOld "floskell"))
       formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
       documentContents doc >>= liftIO . (@?= formattedFloskell)
    ]


ormoluTests :: TestTree
ormoluTests = testGroup "ormolu"
  [ goldenGitDiff "formats correctly" "test/testdata/format/Format.ormolu.formatted.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
      doc <- openDoc "Format.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenGitDiff "formats imports correctly" "test/testdata/format/Format2.ormolu.formatted.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
      doc <- openDoc "Format2.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  ]

fourmoluTests :: TestTree
fourmoluTests = testGroup "fourmolu"
  [ goldenGitDiff "formats correctly" "test/testdata/format/Format.fourmolu.formatted.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "fourmolu"))
      doc <- openDoc "Format.hs" "haskell"
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenGitDiff "formats imports correctly" "test/testdata/format/Format2.fourmolu.formatted.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "fourmolu"))
      doc <- openDoc "Format2.hs" "haskell"
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  ]

formatLspConfig :: Value -> Value
formatLspConfig provider = object [ "haskell" .= object ["formattingProvider" .= (provider :: Value)] ]

-- | The same as 'formatLspConfig' but using the legacy section name
formatLspConfigOld :: Value -> Value
formatLspConfigOld provider = object [ "languageServerHaskell" .= object ["formattingProvider" .= (provider :: Value)] ]

formatConfig :: Value -> SessionConfig
formatConfig provider = defaultConfig { lspConfig = Just (formatLspConfig provider) }
