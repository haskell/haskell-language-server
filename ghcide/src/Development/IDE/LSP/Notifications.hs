-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}

module Development.IDE.LSP.Notifications
    ( setHandlersNotifications
    ) where

import           Development.IDE.LSP.Server
import qualified Language.Haskell.LSP.Core        as LSP
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types       as LSP
import qualified Language.Haskell.LSP.Messages    as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP

import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.IDE.Types.Options

import           Control.Monad.Extra
import qualified Data.Aeson                       as A
import           Data.Foldable                    as F
import           Data.Maybe
import qualified Data.HashMap.Strict              as M
import qualified Data.HashSet                     as S
import qualified Data.Text                        as Text

import           Development.IDE.Core.FileStore   (setSomethingModified, setFileModified, typecheckParents)
import           Development.IDE.Core.FileExists  (modifyFileExists, watchedGlobs)
import           Development.IDE.Core.OfInterest


whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

setHandlersNotifications :: PartialHandlers c
setHandlersNotifications = PartialHandlers $ \WithMessage{..} x -> return x
    {LSP.didOpenTextDocumentNotificationHandler = withNotification (LSP.didOpenTextDocumentNotificationHandler x) $
        \_ ide (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> do
            updatePositionMapping ide (VersionedTextDocumentIdentifier _uri (Just _version)) (List [])
            whenUriFile _uri $ \file -> do
                -- We don't know if the file actually exists, or if the contents match those on disk
                -- For example, vscode restores previously unsaved contents on open
                modifyFilesOfInterest ide (M.insert file Modified)
                setFileModified ide False file
                logInfo (ideLogger ide) $ "Opened text document: " <> getUri _uri

    ,LSP.didChangeTextDocumentNotificationHandler = withNotification (LSP.didChangeTextDocumentNotificationHandler x) $
        \_ ide (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> do
            updatePositionMapping ide identifier changes
            whenUriFile _uri $ \file -> do
              modifyFilesOfInterest ide (M.insert file Modified)
              setFileModified ide False file
            logInfo (ideLogger ide) $ "Modified text document: " <> getUri _uri

    ,LSP.didSaveTextDocumentNotificationHandler = withNotification (LSP.didSaveTextDocumentNotificationHandler x) $
        \_ ide (DidSaveTextDocumentParams TextDocumentIdentifier{_uri}) -> do
            whenUriFile _uri $ \file -> do
                modifyFilesOfInterest ide (M.insert file OnDisk)
                setFileModified ide True file
            logInfo (ideLogger ide) $ "Saved text document: " <> getUri _uri

    ,LSP.didCloseTextDocumentNotificationHandler = withNotification (LSP.didCloseTextDocumentNotificationHandler x) $
        \_ ide (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> do
            whenUriFile _uri $ \file -> do
                modifyFilesOfInterest ide (M.delete file)
                -- Refresh all the files that depended on this
                IdeOptions{optCheckParents} <- getIdeOptionsIO $ shakeExtras ide
                when (optCheckParents >= CheckOnClose) $ typecheckParents ide file
                logInfo (ideLogger ide) $ "Closed text document: " <> getUri _uri
    ,LSP.didChangeWatchedFilesNotificationHandler = withNotification (LSP.didChangeWatchedFilesNotificationHandler x) $
        \_ ide (DidChangeWatchedFilesParams fileEvents) -> do
            -- See Note [File existence cache and LSP file watchers] which explains why we get these notifications and
            -- what we do with them
            let events =
                    mapMaybe
                        (\(FileEvent uri ev) ->
                            (, ev /= FcDeleted) . toNormalizedFilePath'
                            <$> LSP.uriToFilePath uri
                        )
                        ( F.toList fileEvents )
            let msg = Text.pack $ show events
            logDebug (ideLogger ide) $ "Files created or deleted: " <> msg
            modifyFileExists ide events
            setSomethingModified ide

    ,LSP.didChangeWorkspaceFoldersNotificationHandler = withNotification (LSP.didChangeWorkspaceFoldersNotificationHandler x) $
        \_ ide (DidChangeWorkspaceFoldersParams events) -> do
            let add       = S.union
                substract = flip S.difference
            modifyWorkspaceFolders ide
              $ add       (foldMap (S.singleton . parseWorkspaceFolder) (_added   events))
              . substract (foldMap (S.singleton . parseWorkspaceFolder) (_removed events))

    ,LSP.didChangeConfigurationParamsHandler = withNotification (LSP.didChangeConfigurationParamsHandler x) $
        \_ ide (DidChangeConfigurationParams cfg) -> do
            let msg = Text.pack $ show cfg
            logInfo (ideLogger ide) $ "Configuration changed: " <> msg
            modifyClientSettings ide (const $ Just cfg)
            setSomethingModified ide

    -- Initialized handler, good time to dynamically register capabilities
    ,LSP.initializedHandler = withNotification (LSP.initializedHandler x) $ \lsp@LSP.LspFuncs{..} ide _ -> do
        let watchSupported = case () of
              _ | LSP.ClientCapabilities{_workspace} <- clientCapabilities
                , Just LSP.WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
                , Just LSP.DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
                , Just True <- _dynamicRegistration
                  -> True
                | otherwise -> False

        if watchSupported
        then registerWatcher lsp ide
        else logDebug (ideLogger ide) "Warning: Client does not support watched files. Falling back to OS polling"

    }
    where
        registerWatcher LSP.LspFuncs{..} ide = do
            lspId <- getNextReqId
            opts <- getIdeOptionsIO $ shakeExtras ide
            let
              req = RequestMessage "2.0" lspId ClientRegisterCapability regParams
              regParams    = RegistrationParams (List [registration])
              -- The registration ID is arbitrary and is only used in case we want to deregister (which we won't).
              -- We could also use something like a random UUID, as some other servers do, but this works for
              -- our purposes.
              registration = Registration "globalFileWatches"
                                          WorkspaceDidChangeWatchedFiles
                                          (Just (A.toJSON regOptions))
              regOptions =
                DidChangeWatchedFilesRegistrationOptions { _watchers = List watchers }
              -- See Note [File existence cache and LSP file watchers] for why this exists, and the choice of watch kind
              watchKind = WatchKind { _watchCreate = True, _watchChange = False, _watchDelete = True}
              -- See Note [Which files should we watch?] for an explanation of why the pattern is the way that it is
              -- The patterns will be something like "**/.hs", i.e. "any number of directory segments,
              -- followed by a file with an extension 'hs'.
              watcher glob = FileSystemWatcher { _globPattern = glob, _kind = Just watchKind }
              -- We use multiple watchers instead of one using '{}' because lsp-test doesn't
              -- support that: https://github.com/bubba/lsp-test/issues/77
              watchers = [ watcher glob | glob <- watchedGlobs opts ]

            sendFunc $ LSP.ReqRegisterCapability req
