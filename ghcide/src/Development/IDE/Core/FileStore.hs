-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Core.FileStore(
    getFileModTimeContents,
    getFileContents,
    getUriContents,
    getVersionedTextDoc,
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
    shareFilePath,
    Log(..),
    ) where

import           Control.Concurrent.STM.Stats                 (STM, atomically)
import           Control.Concurrent.STM.TQueue                (writeTQueue)
import           Control.Exception
import           Control.Lens                                 ((^.))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.Binary                                  as B
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.HashMap.Strict                          as HashMap
import           Data.IORef
import           Data.Maybe                                   (fromMaybe)
import qualified Data.Text                                    as T
import qualified Data.Text                                    as Text
import           Data.Text.Utf16.Rope.Mixed                   (Rope)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Traversable                             (for)
import           Development.IDE.Core.FileUtils
import           Development.IDE.Core.IdeConfiguration        (isWorkspaceFile)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake                   hiding (Log)
import qualified Development.IDE.Core.Shake                   as Shake
import           Development.IDE.GHC.Orphans                  ()
import           Development.IDE.Graph
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           Development.IDE.Types.Shake                  (toKey)
import           HieDb.Create                                 (deleteMissingRealFiles)
import           Ide.Logger                                   (Pretty (pretty),
                                                               Priority (Info),
                                                               Recorder,
                                                               WithPriority,
                                                               cmapWithPrio,
                                                               logWith, viaShow,
                                                               (<+>))
import qualified Ide.Logger                                   as L
import           Ide.Types
import qualified Language.LSP.Protocol.Lens                   as L
import           Language.LSP.Protocol.Message                (toUntypedRegistration)
import qualified Language.LSP.Protocol.Message                as LSP
import           Language.LSP.Protocol.Types                  (DidChangeWatchedFilesRegistrationOptions (DidChangeWatchedFilesRegistrationOptions),
                                                               FileSystemWatcher (..),
                                                               TextDocumentIdentifier (..),
                                                               VersionedTextDocumentIdentifier (..),
                                                               _watchers,
                                                               uriToNormalizedFilePath)
import qualified Language.LSP.Protocol.Types                  as LSP
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.VFS
import           System.FilePath
import           System.IO.Error
import           System.IO.Unsafe

data Log
  = LogCouldNotIdentifyReverseDeps !NormalizedUri
  | LogTypeCheckingReverseDeps !NormalizedUri !(Maybe [NormalizedUri])
  | LogShake Shake.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogCouldNotIdentifyReverseDeps path ->
      "Could not identify reverse dependencies for" <+> viaShow path
    (LogTypeCheckingReverseDeps path reverseDepPaths) ->
      "Typechecking reverse dependencies for"
      <+> viaShow path
      <> ":"
      <+> pretty (fmap (fmap show) reverseDepPaths)
    LogShake msg -> pretty msg

addWatchedFileRule :: Recorder (WithPriority Log) -> (NormalizedUri -> Action Bool) -> Rules ()
addWatchedFileRule recorder isWatched = defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \AddWatchedFile uri -> do
  isAlreadyWatched <- isWatched uri
  let mfp = uriToNormalizedFilePath uri
  isWp <- fromMaybe False <$> traverse isWorkspaceFile mfp
  if isAlreadyWatched then pure (Just True) else
    if not isWp then pure (Just False) else do
        ShakeExtras{lspEnv} <- getShakeExtras
        case lspEnv of
            Just env -> fmap Just $ liftIO $ LSP.runLspT env $
                fmap (fromMaybe False) $ for mfp $ \fp ->
                  registerFileWatches [fromNormalizedFilePath fp]
            Nothing -> pure $ Just False


getModificationTimeRule :: Recorder (WithPriority Log) -> Rules ()
getModificationTimeRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \(GetModificationTime_ missingFileDiags) uri ->
    getModificationTimeImpl missingFileDiags uri

getModificationTimeImpl
  :: Bool
  -> NormalizedUri
  -> Action (Maybe BS.ByteString, ([FileDiagnostic], Maybe FileVersion))
getModificationTimeImpl missingFileDiags nuri = do
    let uri = fromNormalizedUri nuri
    let wrap time = (Just $ LBS.toStrict $ B.encode $ toRational time, ([], Just $ ModificationTime time))
    mbVf <- getVirtualFile nuri
    case mbVf of
        Just (virtualFileVersion -> ver) -> do
            alwaysRerun
            pure (Just $ LBS.toStrict $ B.encode ver, ([], Just $ VFSVersion ver))
        Nothing -> do
            isWF <- use_ AddWatchedFile nuri
            if isWF
                then -- the file is watched so we can rely on FileWatched notifications,
                        -- but also need a dependency on IsFileOfInterest to reinstall
                        -- alwaysRerun when the file becomes VFS
                    void (use_ IsFileOfInterest nuri)
                else if isInterface nuri
                    then -- interface files are tracked specially using the closed world assumption
                        pure ()
                    else -- in all other cases we will need to freshly check the file system
                        alwaysRerun

            case LSP.uriToFilePath uri of
              -- NOTE: if the URI is *not* in the virtual file system but is also not a file URI, then
              -- we have no other choice but failing - in future it might be possible to resolve different
              -- kinds of URIs here.
              Nothing -> pure (Nothing, ([ideErrorText nuri "Uri is not a fileuri"], Nothing))
              Just f -> do
                liftIO $ fmap wrap (getModTime f)
                    `catch` \(e :: IOException) -> do
                        let err | isDoesNotExistError e = "File does not exist: " ++ f
                                | otherwise = "IO error while reading " ++ f ++ ", " ++ displayException e
                            diag = ideErrorText nuri (T.pack err)
                        if isDoesNotExistError e && not missingFileDiags
                            then return (Nothing, ([], Nothing))
                            else return (Nothing, ([diag], Nothing))


getPhysicalModificationTimeRule :: Recorder (WithPriority Log) -> Rules ()
getPhysicalModificationTimeRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ Rule $ \GetPhysicalModificationTime file ->
    getPhysicalModificationTimeImpl file

getPhysicalModificationTimeImpl
  :: NormalizedUri
  -> Action (Maybe BS.ByteString, ([FileDiagnostic], Maybe FileVersion))
getPhysicalModificationTimeImpl file = do
    let wrap time = (Just $ LBS.toStrict $ B.encode $ toRational time, ([], Just $ ModificationTime time))

    alwaysRerun

    case uriToNormalizedFilePath file of
      Just nfp -> do
        let fp = fromNormalizedFilePath nfp
        liftIO $ fmap wrap (getModTime fp)
            `catch` \(e :: IOException) -> do
                let err | isDoesNotExistError e = "File does not exist: " ++ fp
                        | otherwise = "IO error while reading " ++ fp ++ ", " ++ displayException e
                    diag = ideErrorText file (T.pack err)
                if isDoesNotExistError e
                    then return (Nothing, ([], Nothing))
                    else return (Nothing, ([diag], Nothing))
      Nothing -> pure (Nothing, ([], Nothing))

-- | Interface files cannot be watched, since they live outside the workspace.
--   But interface files are private, in that only HLS writes them.
--   So we implement watching ourselves, and bypass the need for alwaysRerun.
isInterface :: NormalizedUri -> Bool
isInterface uri = case uriToNormalizedFilePath uri of
  Nothing -> False
  Just f -> takeExtension (fromNormalizedFilePath f) `elem` [".hi", ".hi-boot", ".hie", ".hie-boot", ".core"]

-- | Reset the GetModificationTime state of interface files
resetInterfaceStore :: ShakeExtras -> NormalizedUri -> STM [Key]
resetInterfaceStore state uri = deleteValue state GetModificationTime uri

-- | Reset the GetModificationTime state of watched files
--   Assumes the list does not include any FOIs
resetFileStore :: IdeState -> [(NormalizedUri, LSP.FileChangeType)] -> IO [Key]
resetFileStore ideState changes = mask $ \_ -> do
    -- we record FOIs document versions in all the stored values
    -- so NEVER reset FOIs to avoid losing their versions
    -- FOI filtering is done by the caller (LSP Notification handler)
    fmap concat <$>
        forM changes $ \(nfp, c) -> do
            case c of
                LSP.FileChangeType_Changed
                    --  already checked elsewhere |  not $ HM.member nfp fois
                    ->
                      atomically $ do
                        ks <- deleteValue (shakeExtras ideState) GetModificationTime nfp
                        vs <- deleteValue (shakeExtras ideState) GetPhysicalModificationTime nfp
                        pure $ ks ++ vs
                _ -> pure []


modificationTime :: FileVersion -> Maybe UTCTime
modificationTime VFSVersion{}             = Nothing
modificationTime (ModificationTime posix) = Just $ posixSecondsToUTCTime posix

getFileContentsRule :: Recorder (WithPriority Log) -> Rules ()
getFileContentsRule recorder = define (cmapWithPrio LogShake recorder) $ \GetFileContents uri -> getFileContentsImpl uri

getFileContentsImpl
    :: NormalizedUri
    -> Action ([FileDiagnostic], Maybe (FileVersion, Maybe Rope))
getFileContentsImpl uri = do
    -- need to depend on modification time to introduce a dependency with Cutoff
    time <- use_ GetModificationTime uri
    res <- do
        mbVirtual <- getVirtualFile uri
        pure $ _file_text <$> mbVirtual
    pure ([], Just (time, res))

-- | Returns the modification time and the contents.
--   For VFS paths, the modification time is the current time.
getFileModTimeContents :: NormalizedUri -> Action (UTCTime, Maybe Rope)
getFileModTimeContents uri = do
    (fv, contents) <- use_ GetFileContents uri
    modTime <- case modificationTime fv of
      Just t -> pure t
      Nothing -> do
        foi <- use_ IsFileOfInterest uri
        liftIO $ case foi of
          IsFOI Modified{} -> getCurrentTime
          _ | Just nfp <- uriToNormalizedFilePath uri -> do
            posix <- getModTime $ fromNormalizedFilePath nfp
            pure $ posixSecondsToUTCTime posix
          _ -> getCurrentTime
    return (modTime, contents)

getFileContents :: NormalizedUri -> Action (Maybe Rope)
getFileContents = getUriContents

getUriContents :: NormalizedUri -> Action (Maybe Rope)
getUriContents uri = snd <$> use_ GetFileContents uri

-- | Given a text document identifier, annotate it with the latest version.
--
-- Like Language.LSP.Server.Core.getVersionedTextDoc, but gets the virtual file
-- from the Shake VFS rather than the LSP VFS.
getVersionedTextDoc :: TextDocumentIdentifier -> Action VersionedTextDocumentIdentifier
getVersionedTextDoc doc = do
  let uri = doc ^. L.uri
  vf <- getVirtualFile $ toNormalizedUri uri
  let ver = case vf of
        Just (VirtualFile lspver _ _) -> lspver
        Nothing                       -> 0
  return (VersionedTextDocumentIdentifier uri ver)

fileStoreRules :: Recorder (WithPriority Log) -> (NormalizedUri -> Action Bool) -> Rules ()
fileStoreRules recorder isWatched = do
    getModificationTimeRule recorder
    getPhysicalModificationTimeRule recorder
    getFileContentsRule recorder
    addWatchedFileRule recorder isWatched

-- | Note that some buffer for a specific file has been modified but not
-- with what changes.
setFileModified :: Recorder (WithPriority Log)
                -> VFSModified
                -> IdeState
                -> Bool -- ^ Was the file saved?
                -> NormalizedUri
                -> IO [Key]
                -> IO ()
setFileModified recorder vfs state saved nuri actionBefore = do
    ideOptions <- getIdeOptionsIO $ shakeExtras state
    doCheckParents <- optCheckParents ideOptions
    let checkParents = case doCheckParents of
          AlwaysCheck -> True
          CheckOnSave -> saved
          _           -> False
    restartShakeSession (shakeExtras state) vfs (Text.unpack (getUri (fromNormalizedUri nuri)) ++ " (modified)") [] $ do
        keys<-actionBefore
        return (toKey GetModificationTime nuri : keys)
    when checkParents $
      typecheckParents recorder state nuri

typecheckParents :: Recorder (WithPriority Log) -> IdeState -> NormalizedUri -> IO ()
typecheckParents recorder state nuri = void $ shakeEnqueue (shakeExtras state) parents
  where parents = mkDelayedAction "ParentTC" L.Debug (typecheckParentsAction recorder nuri)

typecheckParentsAction :: Recorder (WithPriority Log) -> NormalizedUri -> Action ()
typecheckParentsAction recorder nuri = do
    revs <- transitiveReverseDependencies nuri <$> useWithSeparateFingerprintRule_ GetModuleGraphTransReverseDepsFingerprints GetModuleGraph nuri
    case revs of
      Nothing -> logWith recorder Info $ LogCouldNotIdentifyReverseDeps nuri
      Just rs -> do
        logWith recorder Info $ LogTypeCheckingReverseDeps nuri revs
        void $ uses GetModIface rs

-- | Note that some keys have been modified and restart the session
--   Only valid if the virtual file system was initialised by LSP, as that
--   independently tracks which files are modified.
setSomethingModified :: VFSModified -> IdeState -> String -> IO [Key] -> IO ()
setSomethingModified vfs state reason actionBetweenSession = do
    -- Update database to remove any files that might have been renamed/deleted
    atomically $ writeTQueue (indexQueue $ hiedbWriter $ shakeExtras state) (\withHieDb -> withHieDb deleteMissingRealFiles)
    void $ restartShakeSession (shakeExtras state) vfs reason [] actionBetweenSession

registerFileWatches :: [String] -> LSP.LspT Config IO Bool
registerFileWatches globs = do
      watchSupported <- isWatchSupported
      if watchSupported
      then do
        let
          regParams    = LSP.RegistrationParams  [toUntypedRegistration registration]
          -- The registration ID is arbitrary and is only used in case we want to deregister (which we won't).
          -- We could also use something like a random UUID, as some other servers do, but this works for
          -- our purposes.
          registration = LSP.TRegistration { _id ="globalFileWatches"
                                           , _method = LSP.SMethod_WorkspaceDidChangeWatchedFiles
                                           , _registerOptions = Just regOptions}
          regOptions =
            DidChangeWatchedFilesRegistrationOptions { _watchers = watchers }
          -- See Note [File existence cache and LSP file watchers] for why this exists, and the choice of watch kind
          -- WatchKind_Custom 7 is for create, change, and delete
          watchKind = LSP.WatchKind_Custom 7
          -- See Note [Which files should we watch?] for an explanation of why the pattern is the way that it is
          -- The patterns will be something like "**/.hs", i.e. "any number of directory segments,
          -- followed by a file with an extension 'hs'.
          watcher glob = FileSystemWatcher { _globPattern = glob, _kind = Just watchKind }
          -- We use multiple watchers instead of one using '{}' because lsp-test doesn't
          -- support that: https://github.com/bubba/lsp-test/issues/77
          watchers = [ watcher (LSP.GlobPattern (LSP.InL (LSP.Pattern (Text.pack glob)))) | glob <- globs ]

        void $ LSP.sendRequest LSP.SMethod_ClientRegisterCapability regParams (const $ pure ()) -- TODO handle response
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

filePathMap :: IORef (HashMap.HashMap FilePath FilePath)
filePathMap = unsafePerformIO $ newIORef HashMap.empty
{-# NOINLINE filePathMap #-}

shareFilePath :: FilePath -> FilePath
shareFilePath k = unsafePerformIO $ do
  atomicModifyIORef' filePathMap $ \km ->
    let new_key = HashMap.lookup k km
    in case new_key of
          Just v  -> (km, v)
          Nothing -> (HashMap.insert k k km, k)
{-# NOINLINE shareFilePath  #-}
