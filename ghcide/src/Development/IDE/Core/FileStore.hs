-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Core.FileStore(
    getFileContents,
    setFileModified,
    setSomethingModified,
    fileStoreRules,
    modificationTime,
    typecheckParents,
    resetFileStore,
    resetInterfaceStore,
    getModificationTimeImpl,
    addIdeGlobal,
    getFileContentsImpl,
    getModTime,
    isWatchSupported,
    registerFileWatches,
    Log(..)
    ) where

import           Control.Concurrent.STM.Stats                 (STM, atomically,
                                                               modifyTVar')
import           Control.Concurrent.STM.TQueue                (writeTQueue)
import           Control.Exception
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.ByteString                              as BS
import           Data.Either.Extra
import qualified Data.Rope.UTF16                              as Rope
import qualified Data.Text                                    as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake                   hiding (Log)
import           Development.IDE.Core.FileUtils
import           Development.IDE.GHC.Orphans                  ()
import           Development.IDE.Graph
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           HieDb.Create                                 (deleteMissingRealFiles)
import           Ide.Plugin.Config                            (CheckParents (..),
                                                               Config)
import           System.IO.Error

#ifdef mingw32_HOST_OS
import qualified System.Directory                             as Dir
#else
#endif

import qualified Development.IDE.Types.Logger                 as L

import qualified Data.Binary                                  as B
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.HashSet                                 as HSet
import           Data.List                                    (foldl')
import qualified Data.Text                                    as Text
import           Development.IDE.Core.IdeConfiguration        (isWorkspaceFile)
import qualified Development.IDE.Core.Shake                   as Shake
import           Development.IDE.Types.Logger                 (Pretty (pretty),
                                                               Priority (Info),
                                                               Recorder,
                                                               WithPriority,
                                                               cmapWithPrio,
                                                               logWith, viaShow,
                                                               (<+>))
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.Types                           (DidChangeWatchedFilesRegistrationOptions (DidChangeWatchedFilesRegistrationOptions),
                                                               FileChangeType (FcChanged),
                                                               FileSystemWatcher (..),
                                                               WatchKind (..),
                                                               _watchers)
import qualified Language.LSP.Types                           as LSP
import qualified Language.LSP.Types.Capabilities              as LSP
import           Language.LSP.VFS
import           System.FilePath

data Log
  = LogCouldNotIdentifyReverseDeps !NormalizedFilePath
  | LogTypeCheckingReverseDeps !NormalizedFilePath !(Maybe [NormalizedFilePath])
  | LogShake Shake.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogCouldNotIdentifyReverseDeps path ->
      "Could not identify reverse dependencies for" <+> viaShow path
    (LogTypeCheckingReverseDeps path reverseDepPaths) ->
      "Typechecking reverse dependecies for"
      <+> viaShow path
      <> ":"
      <+> pretty (fmap (fmap show) reverseDepPaths)
    LogShake log -> pretty log

addWatchedFileRule :: Recorder (WithPriority Log) -> (NormalizedFilePath -> Action Bool) -> Rules ()
addWatchedFileRule recorder isWatched = defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \AddWatchedFile f -> do
  isAlreadyWatched <- isWatched f
  isWp <- isWorkspaceFile f
  if isAlreadyWatched then pure (Just True) else
    if not isWp then pure (Just False) else do
        ShakeExtras{lspEnv} <- getShakeExtras
        case lspEnv of
            Just env -> fmap Just $ liftIO $ LSP.runLspT env $
                registerFileWatches [fromNormalizedFilePath f]
            Nothing -> pure $ Just False


getModificationTimeRule :: Recorder (WithPriority Log) -> Rules ()
getModificationTimeRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \(GetModificationTime_ missingFileDiags) file ->
    getModificationTimeImpl missingFileDiags file

getModificationTimeImpl
  :: Bool
  -> NormalizedFilePath
  -> Action (Maybe BS.ByteString, ([FileDiagnostic], Maybe FileVersion))
getModificationTimeImpl missingFileDiags file = do
    let file' = fromNormalizedFilePath file
    let wrap time = (Just $ LBS.toStrict $ B.encode $ toRational time, ([], Just $ ModificationTime time))
    mbVf <- getVirtualFile file
    case mbVf of
        Just (virtualFileVersion -> ver) -> do
            alwaysRerun
            pure (Just $ LBS.toStrict $ B.encode ver, ([], Just $ VFSVersion ver))
        Nothing -> do
            isWF <- use_ AddWatchedFile file
            if isWF
                then -- the file is watched so we can rely on FileWatched notifications,
                        -- but also need a dependency on IsFileOfInterest to reinstall
                        -- alwaysRerun when the file becomes VFS
                    void (use_ IsFileOfInterest file)
                else if isInterface file
                    then -- interface files are tracked specially using the closed world assumption
                        pure ()
                    else -- in all other cases we will need to freshly check the file system
                        alwaysRerun

            liftIO $ fmap wrap (getModTime file')
                `catch` \(e :: IOException) -> do
                    let err | isDoesNotExistError e = "File does not exist: " ++ file'
                            | otherwise = "IO error while reading " ++ file' ++ ", " ++ displayException e
                        diag = ideErrorText file (T.pack err)
                    if isDoesNotExistError e && not missingFileDiags
                        then return (Nothing, ([], Nothing))
                        else return (Nothing, ([diag], Nothing))

-- | Interface files cannot be watched, since they live outside the workspace.
--   But interface files are private, in that only HLS writes them.
--   So we implement watching ourselves, and bypass the need for alwaysRerun.
isInterface :: NormalizedFilePath -> Bool
isInterface f = takeExtension (fromNormalizedFilePath f) `elem` [".hi", ".hi-boot"]

-- | Reset the GetModificationTime state of interface files
resetInterfaceStore :: ShakeExtras -> NormalizedFilePath -> STM ()
resetInterfaceStore state f = do
    deleteValue state GetModificationTime f

-- | Reset the GetModificationTime state of watched files
--   Assumes the list does not include any FOIs
resetFileStore :: IdeState -> [(NormalizedFilePath, FileChangeType)] -> IO ()
resetFileStore ideState changes = mask $ \_ -> do
    -- we record FOIs document versions in all the stored values
    -- so NEVER reset FOIs to avoid losing their versions
    -- FOI filtering is done by the caller (LSP Notification handler)
    forM_ changes $ \(nfp, c) -> do
        case c of
            FcChanged
            --  already checked elsewhere |  not $ HM.member nfp fois
              -> atomically $
               deleteValue (shakeExtras ideState) GetModificationTime nfp
            _ -> pure ()


modificationTime :: FileVersion -> Maybe UTCTime
modificationTime VFSVersion{}             = Nothing
modificationTime (ModificationTime posix) = Just $ posixSecondsToUTCTime posix

getFileContentsRule :: Recorder (WithPriority Log) -> Rules ()
getFileContentsRule recorder = define (cmapWithPrio LogShake recorder) $ \GetFileContents file -> getFileContentsImpl file

getFileContentsImpl
    :: NormalizedFilePath
    -> Action ([FileDiagnostic], Maybe (FileVersion, Maybe T.Text))
getFileContentsImpl file = do
    -- need to depend on modification time to introduce a dependency with Cutoff
    time <- use_ GetModificationTime file
    res <- do
        mbVirtual <- getVirtualFile file
        pure $ Rope.toText . _text <$> mbVirtual
    pure ([], Just (time, res))

ideTryIOException :: NormalizedFilePath -> IO a -> IO (Either FileDiagnostic a)
ideTryIOException fp act =
  mapLeft
      (\(e :: IOException) -> ideErrorText fp $ T.pack $ show e)
      <$> try act

-- | Returns the modification time and the contents.
--   For VFS paths, the modification time is the current time.
getFileContents :: NormalizedFilePath -> Action (UTCTime, Maybe T.Text)
getFileContents f = do
    (fv, txt) <- use_ GetFileContents f
    modTime <- case modificationTime fv of
      Just t -> pure t
      Nothing -> do
        foi <- use_ IsFileOfInterest f
        liftIO $ case foi of
          IsFOI Modified{} -> getCurrentTime
          _ -> do
            posix <- getModTime $ fromNormalizedFilePath f
            pure $ posixSecondsToUTCTime posix
    return (modTime, txt)

fileStoreRules :: Recorder (WithPriority Log) -> (NormalizedFilePath -> Action Bool) -> Rules ()
fileStoreRules recorder isWatched = do
    getModificationTimeRule recorder
    getFileContentsRule recorder
    addWatchedFileRule recorder isWatched

-- | Note that some buffer for a specific file has been modified but not
-- with what changes.
setFileModified :: Recorder (WithPriority Log)
                -> IdeState
                -> Bool -- ^ Was the file saved?
                -> NormalizedFilePath
                -> IO ()
setFileModified recorder state saved nfp = do
    ideOptions <- getIdeOptionsIO $ shakeExtras state
    doCheckParents <- optCheckParents ideOptions
    let checkParents = case doCheckParents of
          AlwaysCheck -> True
          CheckOnSave -> saved
          _           -> False
    join $ atomically $ recordDirtyKeys (shakeExtras state) GetModificationTime [nfp]
    restartShakeSession (shakeExtras state) (fromNormalizedFilePath nfp ++ " (modified)") []
    when checkParents $
      typecheckParents recorder state nfp

typecheckParents :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> IO ()
typecheckParents recorder state nfp = void $ shakeEnqueue (shakeExtras state) parents
  where parents = mkDelayedAction "ParentTC" L.Debug (typecheckParentsAction recorder nfp)

typecheckParentsAction :: Recorder (WithPriority Log) -> NormalizedFilePath -> Action ()
typecheckParentsAction recorder nfp = do
    revs <- transitiveReverseDependencies nfp <$> useNoFile_ GetModuleGraph
    let log = logWith recorder
    case revs of
      Nothing -> log Info $ LogCouldNotIdentifyReverseDeps nfp
      Just rs -> do
        log Info $ LogTypeCheckingReverseDeps nfp revs
        void $ uses GetModIface rs

-- | Note that some keys have been modified and restart the session
--   Only valid if the virtual file system was initialised by LSP, as that
--   independently tracks which files are modified.
setSomethingModified :: IdeState -> [Key] -> String -> IO ()
setSomethingModified state keys reason = do
    -- Update database to remove any files that might have been renamed/deleted
    atomically $ do
        writeTQueue (indexQueue $ hiedbWriter $ shakeExtras state) (\withHieDb -> withHieDb deleteMissingRealFiles)
        modifyTVar' (dirtyKeys $ shakeExtras state) $ \x ->
            foldl' (flip HSet.insert) x keys
    void $ restartShakeSession (shakeExtras state) reason []

registerFileWatches :: [String] -> LSP.LspT Config IO Bool
registerFileWatches globs = do
      watchSupported <- isWatchSupported
      if watchSupported
      then do
        let
          regParams    = LSP.RegistrationParams (List [LSP.SomeRegistration registration])
          -- The registration ID is arbitrary and is only used in case we want to deregister (which we won't).
          -- We could also use something like a random UUID, as some other servers do, but this works for
          -- our purposes.
          registration = LSP.Registration "globalFileWatches"
                                           LSP.SWorkspaceDidChangeWatchedFiles
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
          watchers = [ watcher (Text.pack glob) | glob <- globs ]

        void $ LSP.sendRequest LSP.SClientRegisterCapability regParams (const $ pure ()) -- TODO handle response
        return True
      else return False

isWatchSupported :: LSP.LspT Config IO Bool
isWatchSupported = do
      clientCapabilities <- LSP.getClientCapabilities
      pure $ case () of
            _ | LSP.ClientCapabilities{_workspace} <- clientCapabilities
              , Just LSP.WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
              , Just LSP.DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
              , Just True <- _dynamicRegistration
                -> True
              | otherwise -> False
