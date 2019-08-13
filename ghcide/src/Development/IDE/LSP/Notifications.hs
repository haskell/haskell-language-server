-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Development.IDE.LSP.Notifications
    ( setHandlersNotifications
    ) where

import           Language.Haskell.LSP.Types
import           Development.IDE.LSP.Server
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Types as LSP

import Development.IDE.Types.Logger
import Development.IDE.Core.Service
import Development.IDE.Types.Location

import Control.Monad.Extra
import qualified Data.Set                                  as S

import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest


whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath

setHandlersNotifications :: PartialHandlers
setHandlersNotifications = PartialHandlers $ \WithMessage{..} x -> return x
    {LSP.didOpenTextDocumentNotificationHandler = withNotification (LSP.didOpenTextDocumentNotificationHandler x) $
        \_ ide (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> do
            updatePositionMapping ide (VersionedTextDocumentIdentifier _uri (Just _version)) (List [])
            whenUriFile _uri $ \file -> do
                modifyFilesOfInterest ide (S.insert file)
                logInfo (ideLogger ide) $ "Opened text document: " <> getUri _uri

    ,LSP.didChangeTextDocumentNotificationHandler = withNotification (LSP.didChangeTextDocumentNotificationHandler x) $
        \_ ide (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> do
            updatePositionMapping ide identifier changes
            setSomethingModified ide
            logInfo (ideLogger ide) $ "Modified text document: " <> getUri _uri

    ,LSP.didSaveTextDocumentNotificationHandler = withNotification (LSP.didSaveTextDocumentNotificationHandler x) $
        \_ ide (DidSaveTextDocumentParams TextDocumentIdentifier{_uri}) -> do
            setSomethingModified ide
            logInfo (ideLogger ide) $ "Saved text document: " <> getUri _uri

    ,LSP.didCloseTextDocumentNotificationHandler = withNotification (LSP.didCloseTextDocumentNotificationHandler x) $
        \_ ide (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> do
            whenUriFile _uri $ \file -> do
                modifyFilesOfInterest ide (S.delete file)
                logInfo (ideLogger ide) $ "Closed text document: " <> getUri _uri
    }
