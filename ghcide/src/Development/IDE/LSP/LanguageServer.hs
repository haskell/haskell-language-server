-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes #-}

-- WARNING: A copy of DA.Service.Daml.LanguageServer, try to keep them in sync
-- This version removes the daml: handling
module Development.IDE.LSP.LanguageServer
    ( runLanguageServer
    ) where

import           Development.IDE.LSP.Protocol
import           Development.IDE.LSP.Server

import Control.Monad.IO.Class
import qualified Development.IDE.LSP.Definition as LS.Definition
import qualified Development.IDE.LSP.Hover      as LS.Hover
import qualified Development.IDE.Types.Logger as Logger
import Development.IDE.Core.Service
import Development.IDE.Types.Location

import qualified Data.Aeson                                as Aeson
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Set                                  as S
import qualified Data.Text as T

import Development.IDE.Core.FileStore

import qualified Network.URI                               as URI

import qualified System.Exit

import Language.Haskell.LSP.Core (LspFuncs(..))
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.VFS

textShow :: Show a => a -> T.Text
textShow = T.pack . show

------------------------------------------------------------------------
-- Request handlers
------------------------------------------------------------------------

handleRequest
    :: Logger.Handle
    -> IdeState
    -> (forall resp. resp -> ResponseMessage resp)
    -> (ErrorCode -> ResponseMessage ())
    -> ServerRequest
    -> IO FromServerMessage
handleRequest loggerH compilerH makeResponse makeErrorResponse = \case
    Shutdown -> do
      Logger.logInfo loggerH "Shutdown request received, terminating."
      System.Exit.exitSuccess

    KeepAlive -> pure $ RspCustomServer $ makeResponse Aeson.Null

    Definition params -> RspDefinition . makeResponse <$> LS.Definition.handle loggerH compilerH params
    Hover params -> RspHover . makeResponse <$> LS.Hover.handle loggerH compilerH params
    CodeLens _params -> pure $ RspCodeLens $ makeResponse mempty

    req -> do
        Logger.logWarning loggerH ("Method not found" <> T.pack (show req))
        pure $ RspError $ makeErrorResponse MethodNotFound


handleNotification :: LspFuncs () -> Logger.Handle -> IdeState -> ServerNotification -> IO ()
handleNotification lspFuncs loggerH compilerH = \case

    DidOpenTextDocument (DidOpenTextDocumentParams item) -> do
        case URI.parseURI $ T.unpack $ getUri $ _uri (item :: TextDocumentItem) of
          Just uri
              | URI.uriScheme uri == "file:"
              -> handleDidOpenFile item

              | otherwise
              -> Logger.logWarning loggerH $ "Unknown scheme in URI: "
                    <> textShow uri

          _ -> Logger.logSeriousError loggerH $ "Invalid URI in DidOpenTextDocument: "
                    <> textShow (_uri (item :: TextDocumentItem))

    DidChangeTextDocument (DidChangeTextDocumentParams docId _) -> do
        let uri = _uri (docId :: VersionedTextDocumentIdentifier)

        case uriToFilePath' uri of
          Just (toNormalizedFilePath -> filePath) -> do
            mbVirtual <- getVirtualFileFunc lspFuncs $ toNormalizedUri uri
            let contents = maybe "" (Rope.toText . (_text :: VirtualFile -> Rope.Rope)) mbVirtual
            onFileModified compilerH filePath (Just contents)
            Logger.logInfo loggerH
              $ "Updated text document: " <> textShow (fromNormalizedFilePath filePath)

          Nothing ->
            Logger.logSeriousError loggerH
              $ "Invalid file path: " <> textShow (_uri (docId :: VersionedTextDocumentIdentifier))

    DidCloseTextDocument (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) ->
        case URI.parseURI $ T.unpack $ getUri uri of
          Just uri'
              | URI.uriScheme uri' == "file:" -> do
                    Just fp <- pure $ toNormalizedFilePath <$> uriToFilePath' uri
                    handleDidCloseFile fp
              | otherwise -> Logger.logWarning loggerH $ "Unknown scheme in URI: " <> textShow uri

          _ -> Logger.logSeriousError loggerH
                 $    "Invalid URI in DidCloseTextDocument: "
                   <> textShow uri

    DidSaveTextDocument _params ->
      pure ()

    UnknownNotification _method _params -> return ()
  where
    -- Note that the state changes here are not atomic.
    -- When we have parallel compilation we could manage the state
    -- changes in STM so that we can atomically change the state.
    -- Internally it should be done via the IO oracle. See PROD-2808.
    handleDidOpenFile (TextDocumentItem uri _ _ contents) = do
        Just filePath <- pure $ toNormalizedFilePath <$> uriToFilePath' uri
        onFileModified compilerH filePath (Just contents)
        modifyFilesOfInterest compilerH (S.insert filePath)
        Logger.logInfo loggerH $ "Opened text document: " <> textShow filePath

    handleDidCloseFile filePath = do
         Logger.logInfo loggerH $ "Closed text document: " <> textShow (fromNormalizedFilePath filePath)
         onFileModified compilerH filePath Nothing
         modifyFilesOfInterest compilerH (S.delete filePath)

-- | Manages the file store (caching compilation results and unsaved content).
onFileModified
    :: IdeState
    -> NormalizedFilePath
    -> Maybe T.Text
    -> IO ()
onFileModified service fp mbContents = do
    logDebug service $ "File modified " <> T.pack (show fp)
    setBufferModified service fp mbContents

------------------------------------------------------------------------
-- Server execution
------------------------------------------------------------------------

runLanguageServer
    :: Logger.Handle
    -> ((FromServerMessage -> IO ()) -> VFSHandle -> IO IdeState)
    -> IO ()
runLanguageServer loggerH getIdeState = do
    let getHandlers lspFuncs = do
            compilerH <- getIdeState (sendFunc lspFuncs) (makeLSPVFSHandle lspFuncs)
            pure $ Handlers (handleRequest loggerH compilerH) (handleNotification lspFuncs loggerH compilerH)
    liftIO $ runServer loggerH getHandlers
