-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes #-}

module Development.IDE.LSP.Notifications
    ( setHandlersNotifications
    ) where

import           Development.IDE.LSP.Protocol
import           Development.IDE.LSP.Server hiding (runServer)
import qualified Language.Haskell.LSP.Core as LSP

import Development.IDE.Types.Logger
import Development.IDE.Core.Service
import Development.IDE.Types.Location

import qualified Data.Set                                  as S
import qualified Data.Text as T

import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest

import qualified Network.URI                               as URI


textShow :: Show a => a -> T.Text
textShow = T.pack . show


setHandlersNotifications :: WithMessage -> LSP.Handlers -> IO LSP.Handlers
setHandlersNotifications WithMessage{..} x = return x{
    LSP.didOpenTextDocumentNotificationHandler = withNotification $ \ide (DidOpenTextDocumentParams item) -> do
        case URI.parseURI $ T.unpack $ getUri $ _uri (item :: TextDocumentItem) of
          Just uri
              | URI.uriScheme uri == "file:"
              -> handleDidOpenFile ide item

              | otherwise
              -> logWarning (ideLogger ide) $ "Unknown scheme in URI: "
                    <> textShow uri

          _ -> logSeriousError (ideLogger ide) $ "Invalid URI in DidOpenTextDocument: "
                    <> textShow (_uri (item :: TextDocumentItem))

    ,LSP.didChangeTextDocumentNotificationHandler = withNotification $ \ide (DidChangeTextDocumentParams docId _) -> do
        let uri = _uri (docId :: VersionedTextDocumentIdentifier)

        case uriToFilePath' uri of
          Just (toNormalizedFilePath -> filePath) -> do
            onFileModified ide filePath
            logInfo (ideLogger ide)
              $ "Updated text document: " <> textShow (fromNormalizedFilePath filePath)

          Nothing ->
            logSeriousError (ideLogger ide)
              $ "Invalid file path: " <> textShow (_uri (docId :: VersionedTextDocumentIdentifier))

    ,LSP.didCloseTextDocumentNotificationHandler = withNotification $ \ide (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) ->
        case URI.parseURI $ T.unpack $ getUri uri of
          Just uri'
              | URI.uriScheme uri' == "file:" -> do
                    Just fp <- pure $ toNormalizedFilePath <$> uriToFilePath' uri
                    handleDidCloseFile ide fp
              | otherwise -> logWarning (ideLogger ide) $ "Unknown scheme in URI: " <> textShow uri

          _ -> logSeriousError (ideLogger ide)
                 $    "Invalid URI in DidCloseTextDocument: "
                   <> textShow uri

  }
  where
    -- Note that the state changes here are not atomic.
    -- When we have parallel compilation we could manage the state
    -- changes in STM so that we can atomically change the state.
    -- Internally it should be done via the IO oracle. See PROD-2808.
    handleDidOpenFile ide (TextDocumentItem uri _ _ _) = do
        Just filePath <- pure $ toNormalizedFilePath <$> uriToFilePath' uri
        onFileModified ide filePath
        modifyFilesOfInterest ide (S.insert filePath)
        logInfo (ideLogger ide) $ "Opened text document: " <> textShow filePath

    handleDidCloseFile ide filePath = do
         logInfo (ideLogger ide) $ "Closed text document: " <> textShow (fromNormalizedFilePath filePath)
         onFileModified ide filePath
         modifyFilesOfInterest ide (S.delete filePath)


-- | Manages the file store (caching compilation results and unsaved content).
onFileModified
    :: IdeState
    -> NormalizedFilePath
    -> IO ()
onFileModified service fp = do
    logDebug (ideLogger service) $ "File modified " <> T.pack (show fp)
    -- if we get here then we must be using the LSP framework, in which case we don't
    -- need to bother sending file modifications, other than to force the database to rerun
    setBufferModified service fp Nothing
