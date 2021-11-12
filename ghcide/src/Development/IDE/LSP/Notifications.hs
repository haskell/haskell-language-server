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

import           Language.LSP.Types
import qualified Language.LSP.Types                    as LSP

import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict                   as HM
import qualified Data.HashSet                          as S
import qualified Data.Text                             as Text
import           Development.IDE.Core.FileExists       (modifyFileExists,
                                                        watchedGlobs)
import           Development.IDE.Core.FileStore        (registerFileWatches,
                                                        resetFileStore,
                                                        setFileModified,
                                                        setSomethingModified)
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.OfInterest
import           Development.IDE.Core.RuleTypes        (GetClientSettings (..))
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.IDE.Types.Shake           (toKey)
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
          addFileOfInterest ide file Modified{firstOpen=True}
          setFileModified ide False file
          logDebug (ideLogger ide) $ "Opened text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.STextDocumentDidChange $
      \ide _ (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> liftIO $ do
        updatePositionMapping ide identifier changes
        whenUriFile _uri $ \file -> do
          addFileOfInterest ide file Modified{firstOpen=False}
          setFileModified ide False file
        logDebug (ideLogger ide) $ "Modified text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.STextDocumentDidSave $
      \ide _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
        whenUriFile _uri $ \file -> do
            addFileOfInterest ide file OnDisk
            setFileModified ide True file
        logDebug (ideLogger ide) $ "Saved text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.STextDocumentDidClose $
        \ide _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
          whenUriFile _uri $ \file -> do
              deleteFileOfInterest ide file
              let msg = "Closed text document: " <> getUri _uri
              scheduleGarbageCollection ide
              setSomethingModified ide [] $ Text.unpack msg
              logDebug (ideLogger ide) msg

  , mkPluginNotificationHandler LSP.SWorkspaceDidChangeWatchedFiles $
      \ide _ (DidChangeWatchedFilesParams (List fileEvents)) -> liftIO $ do
        -- See Note [File existence cache and LSP file watchers] which explains why we get these notifications and
        -- what we do with them
        -- filter out files of interest, since we already know all about those
        -- filter also uris that do not map to filenames, since we cannot handle them
        filesOfInterest <- getFilesOfInterest ide
        let fileEvents' =
                [ (nfp, event) | (FileEvent uri event) <- fileEvents
                , Just fp <- [uriToFilePath uri]
                , let nfp = toNormalizedFilePath fp
                , not $ HM.member nfp filesOfInterest
                ]
        unless (null fileEvents') $ do
            let msg = show fileEvents'
            logDebug (ideLogger ide) $ "Watched file events: " <> Text.pack msg
            modifyFileExists ide fileEvents'
            resetFileStore ide fileEvents'
            setSomethingModified ide [] msg

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
        setSomethingModified ide [toKey GetClientSettings emptyFilePath] "config change"

  , mkPluginNotificationHandler LSP.SInitialized $ \ide _ _ -> do
      --------- Initialize Shake session --------------------------------------------------------------------
      liftIO $ shakeSessionInit ide

      --------- Set up file watchers ------------------------------------------------------------------------
      opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
        -- See Note [Which files should we watch?] for an explanation of why the pattern is the way that it is
        -- The patterns will be something like "**/.hs", i.e. "any number of directory segments,
        -- followed by a file with an extension 'hs'.
        -- We use multiple watchers instead of one using '{}' because lsp-test doesn't
        -- support that: https://github.com/bubba/lsp-test/issues/77
      let globs = watchedGlobs opts
      success <- registerFileWatches globs
      unless success $
        liftIO $ logDebug (ideLogger ide) "Warning: Client does not support watched files. Falling back to OS polling"
  ]
    }
