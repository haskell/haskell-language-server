-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

module Development.IDE.LSP.Notifications
    ( whenUriFile
    , descriptor
    , Log(..)
    , ghcideNotificationsPluginPriority
    ) where

import qualified Language.LSP.Protocol.Message         as LSP
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types           as LSP

import           Control.Concurrent.STM.Stats          (atomically)
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
import qualified Development.IDE.Core.FileStore        as FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.OfInterest       hiding (Log, LogShake)
import           Development.IDE.Core.Service          hiding (Log, LogShake)
import           Development.IDE.Core.Shake            hiding (Log)
import qualified Development.IDE.Core.Shake            as Shake
import           Development.IDE.Types.Location
import           Ide.Logger
import           Ide.Types
import           Numeric.Natural

data Log
  = LogShake Shake.Log
  | LogFileStore FileStore.Log
  | LogOpenedTextDocument !Uri
  | LogModifiedTextDocument !Uri
  | LogSavedTextDocument !Uri
  | LogClosedTextDocument !Uri
  | LogWatchedFileEvents !Text.Text
  | LogWarnNoWatchedFilesSupport
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake msg     -> pretty msg
    LogFileStore msg -> pretty msg
    LogOpenedTextDocument uri ->  "Opened text document:" <+> pretty (getUri uri)
    LogModifiedTextDocument uri -> "Modified text document:" <+> pretty (getUri uri)
    LogSavedTextDocument uri -> "Saved text document:" <+> pretty (getUri uri)
    LogClosedTextDocument uri -> "Closed text document:" <+> pretty (getUri uri)
    LogWatchedFileEvents msg -> "Watched file events:" <+> pretty msg
    LogWarnNoWatchedFilesSupport -> "Client does not support watched files. Falling back to OS polling"

whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId desc) { pluginNotificationHandlers = mconcat
  [ mkPluginNotificationHandler LSP.SMethod_TextDocumentDidOpen $
      \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
      atomically $ updatePositionMapping ide (VersionedTextDocumentIdentifier _uri _version) []
      whenUriFile _uri $ \file -> do
          -- We don't know if the file actually exists, or if the contents match those on disk
          -- For example, vscode restores previously unsaved contents on open
          setFileModified (cmapWithPrio LogFileStore recorder) (VFSModified vfs) ide False file $
            addFileOfInterest ide file Modified{firstOpen=True}
      logWith recorder Debug $ LogOpenedTextDocument _uri

  , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidChange $
      \ide vfs _ (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> liftIO $ do
        atomically $ updatePositionMapping ide identifier changes
        whenUriFile _uri $ \file -> do
          setFileModified (cmapWithPrio LogFileStore recorder) (VFSModified vfs) ide False file $
            addFileOfInterest ide file Modified{firstOpen=False}
        logWith recorder Debug $ LogModifiedTextDocument _uri

  , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidSave $
      \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
        whenUriFile _uri $ \file -> do
            setFileModified (cmapWithPrio LogFileStore recorder) (VFSModified vfs) ide True file $
                addFileOfInterest ide file OnDisk
        logWith recorder Debug $ LogSavedTextDocument _uri

  , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidClose $
        \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
          whenUriFile _uri $ \file -> do
              let msg = "Closed text document: " <> getUri _uri
              setSomethingModified (VFSModified vfs) ide (Text.unpack msg) $ do
                scheduleGarbageCollection ide
                deleteFileOfInterest ide file
              logWith recorder Debug $ LogClosedTextDocument _uri

  , mkPluginNotificationHandler LSP.SMethod_WorkspaceDidChangeWatchedFiles $
      \ide vfs _ (DidChangeWatchedFilesParams fileEvents) -> liftIO $ do
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
            logWith recorder Debug $ LogWatchedFileEvents (Text.pack msg)
            setSomethingModified (VFSModified vfs) ide msg $ do
                ks1 <- resetFileStore ide fileEvents'
                ks2 <- modifyFileExists ide fileEvents'
                return (ks1 <> ks2)

  , mkPluginNotificationHandler LSP.SMethod_WorkspaceDidChangeWorkspaceFolders $
      \ide _ _ (DidChangeWorkspaceFoldersParams events) -> liftIO $ do
        let add       = S.union
            substract = flip S.difference
        modifyWorkspaceFolders ide
          $ add       (foldMap (S.singleton . parseWorkspaceFolder) (_added   events))
          . substract (foldMap (S.singleton . parseWorkspaceFolder) (_removed events))

  -- Nothing additional to do here beyond what `lsp` does for us, but this prevents
  -- complaints about there being no handler defined
  , mkPluginNotificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration mempty

  , mkPluginNotificationHandler LSP.SMethod_Initialized $ \ide _ _ _ -> do
      --------- Initialize Shake session --------------------------------------------------------------------
      liftIO $ shakeSessionInit (cmapWithPrio LogShake recorder) ide

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
        liftIO $ logWith recorder Warning LogWarnNoWatchedFilesSupport
  ],

    -- The ghcide descriptors should come last'ish so that the notification handlers
    -- (which restart the Shake build) run after everything else
        pluginPriority = ghcideNotificationsPluginPriority
    }
  where
    desc = "Handles basic notifications for ghcide"

ghcideNotificationsPluginPriority :: Natural
ghcideNotificationsPluginPriority = defaultPluginPriority - 900
