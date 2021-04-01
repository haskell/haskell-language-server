-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}

module Development.IDE.LSP.Notifications
    ( whenUriFile
    , descriptor
    ) where

import qualified Language.LSP.Server                   as LSP
import           Language.LSP.Types
import qualified Language.LSP.Types                    as LSP
import qualified Language.LSP.Types.Capabilities       as LSP

import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.IDE.Types.Options

import           Control.Monad.Extra
import qualified Data.HashMap.Strict                   as M
import qualified Data.HashSet                          as S
import qualified Data.Text                             as Text

import           Control.Monad.IO.Class
import           Development.IDE.Core.FileExists       (modifyFileExists,
                                                        watchedGlobs)
import           Development.IDE.Core.FileStore        (resetFileStore,
                                                        setFileModified,
                                                        setSomethingModified,
                                                        typecheckParents)
import           Development.IDE.Core.OfInterest
import           Ide.Plugin.Config                     (CheckParents (CheckOnClose))
import           Ide.Types

whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId) { pluginNotificationHandlers = mconcat
  [ mkPluginNotificationHandler LSP.STextDocumentDidOpen $
      \ide _ (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
      updatePositionMapping ide (VersionedTextDocumentIdentifier _uri (Just _version)) (List [])
      whenUriFile _uri $ \file -> do
          -- We don't know if the file actually exists, or if the contents match those on disk
          -- For example, vscode restores previously unsaved contents on open
          modifyFilesOfInterest ide (M.insert file Modified{firstOpen=True})
          setFileModified ide False file
          logDebug (ideLogger ide) $ "Opened text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.STextDocumentDidChange $
      \ide _ (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> liftIO $ do
        updatePositionMapping ide identifier changes
        whenUriFile _uri $ \file -> do
          modifyFilesOfInterest ide (M.insert file Modified{firstOpen=False})
          setFileModified ide False file
        logDebug (ideLogger ide) $ "Modified text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.STextDocumentDidSave $
      \ide _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
        whenUriFile _uri $ \file -> do
            modifyFilesOfInterest ide (M.insert file OnDisk)
            setFileModified ide True file
        logDebug (ideLogger ide) $ "Saved text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.STextDocumentDidClose $
        \ide _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
          whenUriFile _uri $ \file -> do
              modifyFilesOfInterest ide (M.delete file)
              -- Refresh all the files that depended on this
              checkParents <- optCheckParents =<< getIdeOptionsIO (shakeExtras ide)
              when (checkParents >= CheckOnClose) $ typecheckParents ide file
              logDebug (ideLogger ide) $ "Closed text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.SWorkspaceDidChangeWatchedFiles $
      \ide _ (DidChangeWatchedFilesParams (List fileEvents)) -> liftIO $ do
        -- See Note [File existence cache and LSP file watchers] which explains why we get these notifications and
        -- what we do with them
        let msg = Text.pack $ show fileEvents
        logDebug (ideLogger ide) $ "Watched file events: " <> msg
        modifyFileExists ide fileEvents
        resetFileStore ide fileEvents
        setSomethingModified ide

  , mkPluginNotificationHandler LSP.SWorkspaceDidChangeWorkspaceFolders $
      \ide _ (DidChangeWorkspaceFoldersParams events) -> liftIO $ do
        let add       = S.union
            substract = flip S.difference
        modifyWorkspaceFolders ide
          $ add       (foldMap (S.singleton . parseWorkspaceFolder) (_added   events))
          . substract (foldMap (S.singleton . parseWorkspaceFolder) (_removed events))

  , mkPluginNotificationHandler LSP.SWorkspaceDidChangeConfiguration $
      \ide _ (DidChangeConfigurationParams cfg) -> liftIO $ do
        let msg = Text.pack $ show cfg
        logDebug (ideLogger ide) $ "Configuration changed: " <> msg
        modifyClientSettings ide (const $ Just cfg)
        setSomethingModified ide

  , mkPluginNotificationHandler LSP.SInitialized $ \ide _ _ -> do
      clientCapabilities <- LSP.getClientCapabilities
      let watchSupported = case () of
            _ | LSP.ClientCapabilities{_workspace} <- clientCapabilities
              , Just LSP.WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
              , Just LSP.DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
              , Just True <- _dynamicRegistration
                -> True
              | otherwise -> False
      if watchSupported
      then do
        opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
        let
          regParams    = RegistrationParams (List [SomeRegistration registration])
          -- The registration ID is arbitrary and is only used in case we want to deregister (which we won't).
          -- We could also use something like a random UUID, as some other servers do, but this works for
          -- our purposes.
          registration = Registration "globalFileWatches"
                                      SWorkspaceDidChangeWatchedFiles
                                      regOptions
          regOptions =
            DidChangeWatchedFilesRegistrationOptions { _watchers = List watchers }
          -- See Note [File existence cache and LSP file watchers] for why this exists, and the choice of watch kind
          watchKind = WatchKind { _watchCreate = True, _watchChange = True, _watchDelete = True}
          -- See Note [Which files should we watch?] for an explanation of why the pattern is the way that it is
          -- The patterns will be something like "**/.hs", i.e. "any number of directory segments,
          -- followed by a file with an extension 'hs'.
          watcher glob = FileSystemWatcher { _globPattern = glob, _kind = Just watchKind }
          -- We use multiple watchers instead of one using '{}' because lsp-test doesn't
          -- support that: https://github.com/bubba/lsp-test/issues/77
          watchers = [ watcher (Text.pack glob) | glob <- watchedGlobs opts ]

        void $ LSP.sendRequest SClientRegisterCapability regParams (const $ pure ()) -- TODO handle response
      else liftIO $ logDebug (ideLogger ide) "Warning: Client does not support watched files. Falling back to OS polling"
  ]
    }
