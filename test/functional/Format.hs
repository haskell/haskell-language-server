{-# LANGUAGE OverloadedStrings #-}
module Format (tests) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Hspec.Expectations

tests :: TestTree
tests = testGroup "format document" [
    goldenVsStringDiff "works" goldenGitDiff "test/testdata/Format.formatted_document.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatDoc doc (FormattingOptions 2 True)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , goldenVsStringDiff "works with custom tab size" goldenGitDiff "test/testdata/Format.formatted_document_with_tabsize.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatDoc doc (FormattingOptions 5 True)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , rangeTests
    , providerTests
    , stylishHaskellTests
    , brittanyTests
    , ormoluTests
    ]

rangeTests :: TestTree
rangeTests = testGroup "format range" [
    goldenVsStringDiff "works" goldenGitDiff "test/testdata/Format.formatted_range.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatRange doc (FormattingOptions 2 True) (Range (Position 1 0) (Position 3 10))
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    , goldenVsStringDiff "works with custom tab size" goldenGitDiff "test/testdata/Format.formatted_range_with_tabsize.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Format.hs" "haskell"
        formatRange doc (FormattingOptions 5 True) (Range (Position 4 0) (Position 7 19))
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    ]

providerTests :: TestTree
providerTests = testGroup "formatting provider" [
    testCase "respects none" $ runSessionWithConfig (formatConfig "none") hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Format.hs" "haskell"
        orig <- documentContents doc

        formatDoc doc (FormattingOptions 2 True)
        documentContents doc >>= liftIO . (`shouldBe` orig)

        formatRange doc (FormattingOptions 2 True) (Range (Position 1 0) (Position 3 10))
        documentContents doc >>= liftIO . (`shouldBe` orig)

    , ignoreTestBecause "Broken" $ testCase "can change on the fly" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Format.hs" "haskell"

        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "brittany"))
        formatDoc doc (FormattingOptions 2 True)
        documentContents doc >>= liftIO . (`shouldBe` formattedBrittany)

        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "floskell"))
        formatDoc doc (FormattingOptions 2 True)
        documentContents doc >>= liftIO . (`shouldBe` formattedFloskell)

        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "brittany"))
        formatDoc doc (FormattingOptions 2 True)
        documentContents doc >>= liftIO . (`shouldBe` formattedBrittanyPostFloskell)
    ]

stylishHaskellTests :: TestTree
stylishHaskellTests = testGroup "stylish-haskell" [
  goldenVsStringDiff "formats a document" goldenGitDiff "test/testdata/StylishHaksell.formatted_document.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "stylish-haskell"))
      doc <- openDoc "StylishHaskell.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True)
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  , goldenVsStringDiff "formats a range" goldenGitDiff "test/testdata/StylishHaksell.formatted_range.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "stylish-haskell"))
      doc <- openDoc "StylishHaskell.hs" "haskell"
      formatRange doc (FormattingOptions 2 True) (Range (Position 0 0) (Position 2 21))
      BS.fromStrict . T.encodeUtf8 <$> documentContents doc
  ]

brittanyTests :: TestTree
brittanyTests = testGroup "brittany" [
    goldenVsStringDiff "formats a document with LF endings" goldenGitDiff "test/testdata/BrittanyLF.formatted_document.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyLF.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenVsStringDiff "formats a document with CRLF endings" goldenGitDiff "test/testdata/BrittanyCRLF.formatted_document.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyCRLF.hs" "haskell"
        formatDoc doc (FormattingOptions 4 True)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenVsStringDiff "formats a range with LF endings" goldenGitDiff "test/testdata/BrittanyLF.formatted_range.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyLF.hs" "haskell"
        let range = Range (Position 1 0) (Position 2 22)
        formatRange doc (FormattingOptions 4 True) range
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc

    , goldenVsStringDiff "formats a range with CRLF endings" goldenGitDiff "test/testdata/BrittanyCRLF.formatted_range.hs" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "BrittanyCRLF.hs" "haskell"
        let range = Range (Position 1 0) (Position 2 22)
        formatRange doc (FormattingOptions 4 True) range
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    ]

ormoluTests :: TestTree
ormoluTests = testGroup "ormolu" [
    goldenVsStringDiff "formats correctly" goldenGitDiff ("test/testdata/Format.ormolu." ++ ormoluGoldenSuffix ++ ".hs") $ runSession hieCommand fullCaps "test/testdata" $ do
        let formatLspConfig provider =
                object [ "haskell" .= object ["formattingProvider" .= (provider :: Value)] ]
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
        doc <- openDoc "Format.hs" "haskell"
        formatDoc doc (FormattingOptions 2 True)
        BS.fromStrict . T.encodeUtf8 <$> documentContents doc
    ]
  where
    ormoluGoldenSuffix = case ghcVersion of
      GHC88 -> "formatted"
      GHC86 -> "formatted"
      _ -> "unchanged"


formatLspConfig :: Value -> Value
formatLspConfig provider = object [ "haskell" .= object ["formattingProvider" .= (provider :: Value)] ]

formatConfig :: Value -> SessionConfig
formatConfig provider = defaultConfig { lspConfig = Just (formatLspConfig provider) }

goldenGitDiff :: FilePath -> FilePath -> [String]
goldenGitDiff fRef fNew = ["git", "diff", "--no-index", "--text", "--exit-code", fRef, fNew]


formattedBrittany :: T.Text
formattedBrittany =
  "module Format where\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n\n\
  \data Baz = Baz { a :: Int, b :: String }\n\n"

formattedFloskell :: T.Text
formattedFloskell =
  "module Format where\n\
  \\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n\n\
  \data Baz = Baz { a :: Int, b :: String }\n\n"

formattedBrittanyPostFloskell :: T.Text
formattedBrittanyPostFloskell =
  "module Format where\n\
  \\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n\n\
  \data Baz = Baz { a :: Int, b :: String }\n\n"
