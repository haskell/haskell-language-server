{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Format (tests) where

import           Control.Lens            ((^.))
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Text.Encoding      as T
import qualified Data.Text.IO as T
import           Language.LSP.Test
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens as LSP
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "format document" [
    goldenVsStringDiff "works" goldenGitDiff "test/testdata/format/Format.formatted_document.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , goldenVsStringDiff "works with custom tab size" goldenGitDiff "test/testdata/format/Format.formatted_document_with_tabsize.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatDoc doc (FormattingOptions 5 True Nothing Nothing Nothing)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , rangeTests
    , providerTests
    , stylishHaskellTests
    , ormoluTests
    , fourmoluTests
    ]

rangeTests :: TestTree
rangeTests = testGroup "format range" [
    goldenVsStringDiff "works" goldenGitDiff "test/testdata/format/Format.formatted_range.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatRange doc (FormattingOptions 2 True Nothing Nothing Nothing) (Range (Position 5 0) (Position 7 10))
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , goldenVsStringDiff "works with custom tab size" goldenGitDiff "test/testdata/format/Format.formatted_range_with_tabsize.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatRange doc (FormattingOptions 5 True Nothing Nothing Nothing) (Range (Position 8 0) (Position 11 19))
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    ]

providerTests :: TestTree
providerTests = testGroup "formatting provider" [
    testCase "respects none" $ runSessionWithConfig (formatConfig "none") hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        resp <- request STextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
        liftIO $ resp ^. LSP.result @?= (Left $ ResponseError InvalidRequest "No plugin enabled for STextDocumentFormatting, available: []" Nothing)

    ,  testCase "respects initial" $ runSessionWithConfig (formatConfig "floskell") hlsCommand fullCaps "test/testdata/format" $ do
        doc <- openDoc "Format.hs" "haskell"
        formattedFloskell <- liftIO $ T.readFile "test/testdata/format/Format.floskell.initial.hs"
        formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
        documentContents doc >>= liftIO . (@?= formattedFloskell)

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

stylishHaskellTests :: TestTree
stylishHaskellTests = testGroup "stylish-haskell" [
  goldenVsStringDiff "formats a document" goldenGitDiff "test/testdata/format/StylishHaskell.formatted_document.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "stylish-haskell"))
      doc <- openDoc "StylishHaskell.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenVsStringDiff "formats a range" goldenGitDiff "test/testdata/format/StylishHaskell.formatted_range.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "stylish-haskell"))
      doc <- openDoc "StylishHaskell.hs" "haskell"
      formatRange doc (FormattingOptions 2 True Nothing Nothing Nothing) (Range (Position 0 0) (Position 2 21))
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  ]

ormoluTests :: TestTree
ormoluTests = testGroup "ormolu"
  [ goldenVsStringDiff "formats correctly" goldenGitDiff "test/testdata/format/Format.ormolu.formatted.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
      doc <- openDoc "Format.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenVsStringDiff "formats imports correctly" goldenGitDiff "test/testdata/format/Format2.ormolu.formatted.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
      doc <- openDoc "Format2.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  ]

fourmoluTests :: TestTree
fourmoluTests = testGroup "fourmolu"
  [ goldenVsStringDiff "formats correctly" goldenGitDiff "test/testdata/format/Format.fourmolu.formatted.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
      sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "fourmolu"))
      doc <- openDoc "Format.hs" "haskell"
      formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenVsStringDiff "formats imports correctly" goldenGitDiff "test/testdata/format/Format2.fourmolu.formatted.hs" $ runSession hlsCommand fullCaps "test/testdata/format" $ do
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

goldenGitDiff :: FilePath -> FilePath -> [String]
goldenGitDiff fRef fNew = ["git", "diff", "--no-index", "--text", "--exit-code", fRef, fNew]
