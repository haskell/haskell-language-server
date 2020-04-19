{-# LANGUAGE OverloadedStrings #-}
module FormatSpec where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
  describe "format document" $ do
    it "works" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True)
      documentContents doc >>= liftIO . (`shouldBe` formattedDocOrmolu)
    it "works with custom tab size" $ do
      pendingWith "ormolu does not accept parameters"
        -- $ runSession hieCommand fullCaps "test/testdata" $ do
      -- doc <- openDoc "Format.hs" "haskell"
      -- formatDoc doc (FormattingOptions 5 True)
      -- documentContents doc >>= liftIO . (`shouldBe` formattedDocTabSize5)

  describe "format range" $ do
    it "works" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"
      formatRange doc (FormattingOptions 2 True) (Range (Position 2 0) (Position 4 10))
      documentContents doc >>= liftIO . (`shouldBe` formattedRangeTabSize2)
    it "works with custom tab size" $ do
      pendingWith "ormolu does not accept parameters"
      --   $ runSession hieCommand fullCaps "test/testdata" $ do
      -- doc <- openDoc "Format.hs" "haskell"
      -- formatRange doc (FormattingOptions 5 True) (Range (Position 4 0) (Position 7 19))
      -- documentContents doc >>= liftIO . (`shouldBe` formattedRangeTabSize5)

  describe "formatting provider" $ do
    let formatLspConfig provider =
          object [ "languageServerHaskell" .= object ["formattingProvider" .= (provider :: Value)] ]
        formatConfig provider = defaultConfig { lspConfig = Just (formatLspConfig provider) }

    it "respects none" $ runSessionWithConfig (formatConfig "none") hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"
      orig <- documentContents doc

      formatDoc doc (FormattingOptions 2 True)
      documentContents doc >>= liftIO . (`shouldBe` orig)

      formatRange doc (FormattingOptions 2 True) (Range (Position 2 0) (Position 4 10))
      documentContents doc >>= liftIO . (`shouldBe` orig)

    -- ---------------------------------

    it "formatting is idempotent" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"

      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
      formatDoc doc (FormattingOptions 2 True)
      documentContents doc >>= liftIO . (`shouldBe` formattedDocOrmolu)

      formatDoc doc (FormattingOptions 2 True)
      liftIO $ pendingWith "documentContents returns junk"
      documentContents doc >>= liftIO . (`shouldBe` formattedDocOrmolu)

    -- ---------------------------------

    it "can change on the fly" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"

      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
      formatDoc doc (FormattingOptions 2 True)
      documentContents doc >>= liftIO . (`shouldBe` formattedDocOrmolu)
      -- sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "brittany"))
      -- formatDoc doc (FormattingOptions 2 True)
      -- documentContents doc >>= liftIO . (`shouldBe` formattedDocTabSize2)

      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "floskell"))
      formatDoc doc (FormattingOptions 2 True)
      liftIO $ pendingWith "documentContents returns junk"
      documentContents doc >>= liftIO . (`shouldBe` formattedFloskellPostBrittany)

      -- sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "brittany"))
      -- formatDoc doc (FormattingOptions 2 True)
      -- documentContents doc >>= liftIO . (`shouldBe` formattedBrittanyPostFloskell)

  describe "brittany" $ do
    let formatLspConfig provider =
          object [ "languageServerHaskell" .= object ["formattingProvider" .= (provider :: Value)] ]
    it "formats a document with LF endings" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "BrittanyLF.hs" "haskell"
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "brittany"))
      let opts = DocumentFormattingParams doc (FormattingOptions 4 True) Nothing
      ResponseMessage _ _ (Just edits) _ <- request TextDocumentFormatting opts
      liftIO $ edits `shouldBe` [TextEdit (Range (Position 0 0) (Position 3 0))
                                  "foo :: Int -> String -> IO ()\nfoo x y = do\n    print x\n    return 42\n"]

    it "formats a document with CRLF endings" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "BrittanyCRLF.hs" "haskell"
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "brittany"))
      let opts = DocumentFormattingParams doc (FormattingOptions 4 True) Nothing
      ResponseMessage _ _ (Just edits) _ <- request TextDocumentFormatting opts
      liftIO $ edits `shouldBe` [TextEdit (Range (Position 0 0) (Position 3 0))
                                  "foo :: Int -> String -> IO ()\nfoo x y = do\n    print x\n    return 42\n"]

    it "formats a range with LF endings" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "BrittanyLF.hs" "haskell"
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "brittany"))
      let range = Range (Position 1 0) (Position 2 22)
          opts = DocumentRangeFormattingParams doc range (FormattingOptions 4 True) Nothing
      ResponseMessage _ _ (Just edits) _ <- request TextDocumentRangeFormatting opts
      liftIO $ edits `shouldBe` [TextEdit (Range (Position 1 0) (Position 3 0))
                                    "foo x y = do\n    print x\n    return 42\n"]

    it "formats a range with CRLF endings" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "BrittanyCRLF.hs" "haskell"
      let range = Range (Position 1 0) (Position 2 22)
          opts = DocumentRangeFormattingParams doc range (FormattingOptions 4 True) Nothing
      ResponseMessage _ _ (Just edits) _ <- request TextDocumentRangeFormatting opts
      liftIO $ edits `shouldBe` [TextEdit (Range (Position 1 0) (Position 3 0))
                                    "foo x y = do\n    print x\n    return 42\n"]

    -- ---------------------------------

  describe "ormolu" $ do
    let formatLspConfig provider =
          object [ "languageServerHaskell" .= object ["formattingProvider" .= (provider :: Value)] ]

    it "formats correctly" $ runSession hieCommand fullCaps "test/testdata" $ do
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (formatLspConfig "ormolu"))
      doc <- openDoc "Format.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True)
      docContent <- documentContents doc
      let formatted = liftIO $ docContent `shouldBe` formattedOrmolu
      case ghcVersion of
        GHC88 -> formatted
        GHC86 -> formatted
        _ -> liftIO $ docContent `shouldBe` unchangedOrmolu

-- ---------------------------------------------------------------------

formattedDocOrmolu :: T.Text
formattedDocOrmolu =
  "{-# LANGUAGE NoImplicitPrelude #-}\n\n\
  \module Format where\n\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n"

formattedDocTabSize2 :: T.Text
formattedDocTabSize2 =
  "module Format where\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n\n"

formattedDocTabSize5 :: T.Text
formattedDocTabSize5 =
  "module Format where\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \     x <- return \"hello\"\n\
  \     return \"asdf\"\n\n"

formattedRangeTabSize2 :: T.Text
formattedRangeTabSize2 =
  "{-# LANGUAGE NoImplicitPrelude #-}\n\
  \module    Format where\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \bar   :: String ->   IO String\n\
  \bar s =  do\n\
  \      x <- return \"hello\"\n\
  \      return \"asdf\"\n\
  \"

formattedRangeTabSize5 :: T.Text
formattedRangeTabSize5 =
  "{-# LANGUAGE NoImplicitPrelude #-}\n\n\
  \module    Format where\n\
  \foo   :: Int ->  Int\n\
  \foo  3 = 2\n\
  \foo    x  = x\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \     x <- return \"hello\"\n\
  \     return \"asdf\"\n\
  \      \n"

formattedFloskell :: T.Text
formattedFloskell =
  "{-# LANGUAGE NoImplicitPrelude #-}\n\n\
  \module Format where\n\
  \\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n\n\
  \"

-- TODO: the format is wrong, but we are currently testing switching formatters only.
--       (duplicated last line)
formattedFloskellPostBrittany :: T.Text
formattedFloskellPostBrittany =
  "{-# LANGUAGE NoImplicitPrelude #-}\n\n\
  \module Format where\n\
  \\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n\
  \"

formattedBrittanyPostFloskell :: T.Text
formattedBrittanyPostFloskell =
  "{-# LANGUAGE NoImplicitPrelude #-}\n\n\
  \module Format where\n\
  \\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n\n"

formattedOrmolu :: T.Text
formattedOrmolu =
  "{-# LANGUAGE NoImplicitPrelude #-}\n\n\
  \module Format where\n\
  \\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n"

unchangedOrmolu :: T.Text
unchangedOrmolu =
  "{-# LANGUAGE NoImplicitPrelude #-}\n\n\
  \module    Format where\n\
  \foo   :: Int ->  Int\n\
  \foo  3 = 2\n\
  \foo    x  = x\n\
  \bar   :: String ->   IO String\n\
  \bar s =  do\n\
  \      x <- return \"hello\"\n\
  \      return \"asdf\"\n\
  \      \n"
