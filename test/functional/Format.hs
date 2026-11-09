{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Format (tests) where

import           Control.Lens                ((^.))
import           Control.Monad.IO.Class
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           HlsPlugins                  (idePlugins)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           Language.LSP.Test
import           System.FilePath.Posix       ((</>))
import           Test.Hls
import qualified Test.Hls.FileSystem         as FS
import           Test.Hls.Flags              (requiresFourmoluPlugin,
                                              requiresOrmoluPlugin)

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
        runSessionWithServerInTmpDir def { formattingProvider = "none" } idePlugins tree $ do
          doc <- openDoc filePath LanguageKind_Haskell
          resp <- request SMethod_TextDocumentFormatting $ DocumentFormattingParams Nothing doc (FormattingOptions 2 True Nothing Nothing Nothing)
          liftIO $ case resp ^. L.result of
            result@(Left (TResponseError reason message Nothing)) -> case reason of
              (InR ErrorCodes_MethodNotFound) -> pure () -- No formatter
              (InR ErrorCodes_InvalidRequest) | "No plugin" `T.isPrefixOf` message -> pure ()
              _ -> assertFailure $ "strange response from formatting provider:" ++ show result
            result -> assertFailure $ "strange response from formatting provider:" ++ show result

    , requiresOrmoluPlugin $ requiresFourmoluPlugin $ testCase "can change on the fly" $ do
        runSessionWithServerInTmpDir def idePlugins tree $ do
          doc <- openDoc filePath LanguageKind_Haskell

          setIgnoringConfigurationRequests False

          setHlsConfig (formatLspConfig "ormolu")
          formatDoc doc defFmtOpts
          formattedOrmolu <- liftIO $ T.readFile "test/testdata/format/Format.ormolu.formatted.hs"
          documentContents doc >>= liftIO . (@?= formattedOrmolu)

          setHlsConfig (formatLspConfig "fourmolu")
          formatDoc doc defFmtOpts { _tabSize = 4 }
          formattedFourmolu <- liftIO $ T.readFile "test/testdata/format/Format.fourmolu.formatted.hs"
          documentContents doc >>= liftIO . (@?= formattedFourmolu)

          setHlsConfig (formatLspConfig "ormolu")
          formatDoc doc defFmtOpts
          formattedOrmoluPostFourmolu <- liftIO $ T.readFile "test/testdata/format/Format.ormolu_post_fourmolu.formatted.hs"
          documentContents doc >>= liftIO . (@?= formattedOrmoluPostFourmolu)
    ]

formatLspConfig :: T.Text -> Config
formatLspConfig provider = def { formattingProvider = provider }

defFmtOpts :: FormattingOptions
defFmtOpts = FormattingOptions { _tabSize = 2
                               , _insertSpaces = True
                               , _trimTrailingWhitespace = Nothing
                               , _insertFinalNewline = Nothing
                               , _trimFinalNewlines = Nothing }
