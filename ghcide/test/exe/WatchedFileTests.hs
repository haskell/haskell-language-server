
{-# LANGUAGE GADTs #-}

module WatchedFileTests (tests) where

import           Config                          (testWithDummyPluginEmpty')
import           Control.Applicative.Combinators
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Aeson                      as A
import qualified Data.Text                       as T
import           Development.IDE.Test            (expectDiagnostics)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           System.Directory
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "watched files"
  [ testGroup "Subscriptions"
    [ testWithDummyPluginEmpty' "workspace files" $ \sessionDir -> do
        liftIO $ writeFile (sessionDir </> "hie.yaml") "cradle: {direct: {arguments: [\"-isrc\", \"A\", \"WatchedFilesMissingModule\"]}}"
        _doc <- createDoc "A.hs" "haskell" "{-#LANGUAGE NoImplicitPrelude #-}\nmodule A where\nimport WatchedFilesMissingModule"
        setIgnoringRegistrationRequests False
        watchedFileRegs <- getWatchedFilesSubscriptionsUntil SMethod_TextDocumentPublishDiagnostics

        -- Expect 2 subscriptions: one for all .hs files and one for the hie.yaml cradle
        liftIO $ length watchedFileRegs @?= 2

    , testWithDummyPluginEmpty' "non workspace file" $ \sessionDir -> do
        tmpDir <- liftIO getTemporaryDirectory
        let yaml = "cradle: {direct: {arguments: [\"-i" <> tail(init(show tmpDir)) <> "\", \"A\", \"WatchedFilesMissingModule\"]}}"
        liftIO $ writeFile (sessionDir </> "hie.yaml") yaml
        _doc <- createDoc "A.hs" "haskell" "{-# LANGUAGE NoImplicitPrelude#-}\nmodule A where\nimport WatchedFilesMissingModule"
        setIgnoringRegistrationRequests False
        watchedFileRegs <- getWatchedFilesSubscriptionsUntil SMethod_TextDocumentPublishDiagnostics

        -- Expect 2 subscriptions: one for all .hs files and one for the hie.yaml cradle
        liftIO $ length watchedFileRegs @?= 2

    -- TODO add a test for didChangeWorkspaceFolder
    ]
  , testGroup "Changes"
    [
      testWithDummyPluginEmpty' "workspace files" $ \sessionDir -> do
        liftIO $ writeFile (sessionDir </> "hie.yaml") "cradle: {direct: {arguments: [\"-isrc\", \"A\", \"B\"]}}"
        liftIO $ writeFile (sessionDir </> "B.hs") $ unlines
          ["module B where"
          ,"b :: Bool"
          ,"b = False"]
        _doc <- createDoc "A.hs" "haskell" $ T.unlines
          ["module A where"
          ,"import B"
          ,"a :: ()"
          ,"a = b"
          ]
        expectDiagnostics [("A.hs", [(DiagnosticSeverity_Error, (3, 4), "Couldn't match expected type '()' with actual type 'Bool'")])]
        -- modify B off editor
        liftIO $ writeFile (sessionDir </> "B.hs") $ unlines
          ["module B where"
          ,"b :: Int"
          ,"b = 0"]
        sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams
               [FileEvent (filePathToUri $ sessionDir </> "B.hs") FileChangeType_Changed ]
        expectDiagnostics [("A.hs", [(DiagnosticSeverity_Error, (3, 4), "Couldn't match expected type '()' with actual type 'Int'")])]
    ]
  ]

getWatchedFilesSubscriptionsUntil :: forall m. SServerMethod m -> Session [DidChangeWatchedFilesRegistrationOptions]
getWatchedFilesSubscriptionsUntil m = do
      msgs <- manyTill (Just <$> message SMethod_ClientRegisterCapability <|> Nothing <$ anyMessage) (message m)
      return
            [ x
            | Just TRequestMessage{_params = RegistrationParams regs} <- msgs
            , Registration _id "workspace/didChangeWatchedFiles" (Just args) <- regs
            , Just x@(DidChangeWatchedFilesRegistrationOptions _) <- [A.decode . A.encode $ args]
            ]
