{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass    #-}
module Format (tests) where

import           Control.Lens                ((^.))
import           Control.Monad.IO.Class
import           Data.Default                (Default)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Development.IDE             (Pretty)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           Language.LSP.Test
import           Test.Hls
import qualified Test.Hls.FileSystem         as FS
#if defined(hls_ormolu)
import           Ide.Logger                  (cmapWithPrio)
import qualified Ide.Plugin.Fourmolu         as Fourmolu
import qualified Ide.Plugin.Ormolu           as Ormolu
import           System.FilePath.Posix       ((</>))
#endif

tests :: TestTree
tests = testGroup "format document"
    [ providerTests
    ]

providerTests :: TestTree
providerTests =
  let filePath = "Format.hs"
      tree = FS.mkVirtualFileTree ("test" </> "testdata" </> "format")
           $ FS.simpleCabalProject' [FS.copy filePath]
  in testGroup "lsp formatting provider"
    [ testCase "respects none" $ do

        runSessionWithServerInTmpDir def (mempty :: PluginTestDescriptor TestLog) tree $ do
          doc <- openDoc filePath LanguageKind_Haskell
          resp <- request SMethod_TextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
          liftIO $ case resp ^. L.result of
            result@(Left (TResponseError reason message Nothing)) -> case reason of
              (InR ErrorCodes_MethodNotFound) -> pure () -- No formatter
              (InR ErrorCodes_InvalidRequest) | "No plugin" `T.isPrefixOf` message -> pure ()
              _ -> assertFailure $ "strange response from formatting provider:" ++ show result
            result -> assertFailure $ "strange response from formatting provider:" ++ show result

#if defined(hls_ormolu)
    , testCase "can change on the fly" $ do
        let orm'and'four'moluPlugins = mkPluginTestDescriptor (Ormolu.descriptor . cmapWithPrio OrmoluLog) "ormolu"
                                    <> mkPluginTestDescriptor (Fourmolu.descriptor . cmapWithPrio FourmoluLog) "fourmolu"

        runSessionWithServerInTmpDir def orm'and'four'moluPlugins tree $ do
          doc <- openDoc filePath LanguageKind_Haskell

          setIgnoringConfigurationRequests False

          setHlsConfig (formatLspConfig "ormolu")
          formatDoc doc def
          formattedOrmolu <- liftIO $ T.readFile "test/testdata/format/Format.ormolu.formatted.hs"
          documentContents doc >>= liftIO . (@?= formattedOrmolu)

          setHlsConfig (formatLspConfig "fourmolu")
          formatDoc doc def { _tabSize = 4 }
          formattedFourmolu <- liftIO $ T.readFile "test/testdata/format/Format.fourmolu.formatted.hs"
          documentContents doc >>= liftIO . (@?= formattedFourmolu)

          setHlsConfig (formatLspConfig "ormolu")
          formatDoc doc def
          formattedOrmoluPostFourmolu <- liftIO $ T.readFile "test/testdata/format/Format.ormolu_post_fourmolu.formatted.hs"
          documentContents doc >>= liftIO . (@?= formattedOrmoluPostFourmolu)
#endif
    ]

data TestLog = OrmoluLog Ormolu.LogEvent
             | FourmoluLog Fourmolu.LogEvent
             deriving (Pretty, Show)

formatLspConfig :: T.Text -> Config
formatLspConfig provider = def { formattingProvider = provider }

-- TODO: this is actually orphan. I wonder if defining it in lsp-types would be reasonable
instance Default FormattingOptions where
  def = FormattingOptions { _tabSize = 2
                          , _insertSpaces = True
                          , _trimTrailingWhitespace = Nothing
                          , _insertFinalNewline = Nothing
                          , _trimFinalNewlines = Nothing }
