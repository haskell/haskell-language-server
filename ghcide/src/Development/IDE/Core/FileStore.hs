-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Core.FileStore(
    getFileContents,
    getVirtualFile,
    setFileModified,
    setSomethingModified,
    fileStoreRules,
    modificationTime,
    typecheckParents,
    VFSHandle,
    makeVFSHandle,
    makeLSPVFSHandle,
    isFileOfInterestRule,
    resetFileStore,
    resetInterfaceStore,
    getModificationTimeImpl,
    addIdeGlobal,
    getFileContentsImpl,
    getModTime
    ) where

import           Control.Concurrent.STM                       (atomically)
import           Control.Concurrent.STM.TQueue                (writeTQueue)
import           Control.Concurrent.Strict
import           Control.Exception
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.ByteString                              as BS
import           Data.Either.Extra
import qualified Data.HashMap.Strict                          as HM
import qualified Data.Map.Strict                              as Map
import           Data.Maybe
import qualified Data.Rope.UTF16                              as Rope
import qualified Data.Text                                    as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Development.IDE.Core.OfInterest              (OfInterestVar (..),
                                                               getFilesOfInterest)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Orphans                  ()
import           Development.IDE.Graph
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           HieDb.Create                                 (deleteMissingRealFiles)
import           Ide.Plugin.Config                            (CheckParents (..))
import           System.IO.Error

#ifdef mingw32_HOST_OS
import qualified System.Directory                             as Dir
#else
import           System.Posix.Files                           (getFileStatus,
                                                               modificationTimeHiRes)
#endif

import qualified Development.IDE.Types.Logger                 as L

import qualified Data.Binary                                  as B
import qualified Data.ByteString.Lazy                         as LBS
import           Language.LSP.Server                          hiding
                                                              (getVirtualFile)
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.Types                           (FileChangeType (FcChanged),
                                                               FileEvent (FileEvent),
                                                               toNormalizedFilePath,
                                                               uriToFilePath)
import           Language.LSP.VFS
import           System.FilePath

makeVFSHandle :: IO VFSHandle
makeVFSHandle = do
    vfsVar <- newVar (1, Map.empty)
    pure VFSHandle
        { getVirtualFile = \uri -> do
              (_nextVersion, vfs) <- readVar vfsVar
              pure $ Map.lookup uri vfs
        , setVirtualFileContents = Just $ \uri content ->
              void $ modifyVar' vfsVar $ \(nextVersion, vfs) -> (nextVersion + 1, ) $
                  case content of
                    Nothing -> Map.delete uri vfs
                    -- The second version number is only used in persistFileVFS which we do not use so we set it to 0.
                    Just content -> Map.insert uri (VirtualFile nextVersion 0 (Rope.fromText content)) vfs
        }

makeLSPVFSHandle :: LanguageContextEnv c -> VFSHandle
makeLSPVFSHandle lspEnv = VFSHandle
    { getVirtualFile = runLspT lspEnv . LSP.getVirtualFile
    , setVirtualFileContents = Nothing
   }


isFileOfInterestRule :: Rules ()
isFileOfInterestRule = defineEarlyCutoff $ RuleNoDiagnostics $ \IsFileOfInterest f -> do
    filesOfInterest <- getFilesOfInterest
    let foi = maybe NotFOI IsFOI $ f `HM.lookup` filesOfInterest
        fp  = summarize foi
        res = (Just fp, Just foi)
    return res
    where
    summarize NotFOI                   = BS.singleton 0
    summarize (IsFOI OnDisk)           = BS.singleton 1
    summarize (IsFOI (Modified False)) = BS.singleton 2
    summarize (IsFOI (Modified True))  = BS.singleton 3


getModificationTimeRule :: VFSHandle -> (NormalizedFilePath -> Action Bool) -> Rules ()
getModificationTimeRule vfs isWatched = defineEarlyCutoff $ Rule $ \(GetModificationTime_ missingFileDiags) file ->
    getModificationTimeImpl vfs isWatched missingFileDiags file

getModificationTimeImpl :: VFSHandle
    -> (NormalizedFilePath -> Action Bool)
    -> Bool
    -> NormalizedFilePath
    -> Action
        (Maybe BS.ByteString, ([FileDiagnostic], Maybe FileVersion))
getModificationTimeImpl vfs isWatched missingFileDiags file = do
        let file' = fromNormalizedFilePath file
        let wrap time = (Just $ LBS.toStrict $ B.encode $ toRational time, ([], Just $ ModificationTime time))
        mbVirtual <- liftIO $ getVirtualFile vfs $ filePathToUri' file
        case mbVirtual of
            Just (virtualFileVersion -> ver) -> do
                alwaysRerun
                pure (Just $ LBS.toStrict $ B.encode ver, ([], Just $ VFSVersion ver))
            Nothing -> do
                isWF <- isWatched file
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
resetInterfaceStore :: ShakeExtras -> NormalizedFilePath -> IO ()
resetInterfaceStore state f = do
    deleteValue state GetModificationTime f

-- | Reset the GetModificationTime state of watched files
resetFileStore :: IdeState -> [FileEvent] -> IO ()
resetFileStore ideState changes = mask $ \_ ->
    forM_ changes $ \(FileEvent uri c) ->
        case c of
            FcChanged
              | Just f <- uriToFilePath uri
              -> do
                  -- we record FOIs document versions in all the stored values
                  -- so NEVER reset FOIs to avoid losing their versions
                  OfInterestVar foisVar <- getIdeGlobalExtras (shakeExtras ideState)
                  fois <- readVar foisVar
                  unless (HM.member (toNormalizedFilePath f) fois) $ do
                    deleteValue (shakeExtras ideState) GetModificationTime (toNormalizedFilePath' f)
            _ -> pure ()

-- Dir.getModificationTime is surprisingly slow since it performs
-- a ton of conversions. Since we do not actually care about
-- the format of the time, we can get away with something cheaper.
-- For now, we only try to do this on Unix systems where it seems to get the
-- time spent checking file modifications (which happens on every change)
-- from > 0.5s to ~0.15s.
-- We might also want to try speeding this up on Windows at some point.
-- TODO leverage DidChangeWatchedFile lsp notifications on clients that
-- support them, as done for GetFileExists
getModTime :: FilePath -> IO POSIXTime
getModTime f =
#ifdef mingw32_HOST_OS
    utcTimeToPOSIXSeconds <$> Dir.getModificationTime f
#else
    modificationTimeHiRes <$> getFileStatus f
#endif

modificationTime :: FileVersion -> Maybe UTCTime
modificationTime VFSVersion{}             = Nothing
modificationTime (ModificationTime posix) = Just $ posixSecondsToUTCTime posix

getFileContentsRule :: VFSHandle -> Rules ()
getFileContentsRule vfs = define $ \GetFileContents file -> getFileContentsImpl vfs file

getFileContentsImpl
    :: VFSHandle
    -> NormalizedFilePath
    -> Action ([FileDiagnostic], Maybe (FileVersion, Maybe T.Text))
getFileContentsImpl vfs file = do
    -- need to depend on modification time to introduce a dependency with Cutoff
    time <- use_ GetModificationTime file
    res <- liftIO $ ideTryIOException file $ do
        mbVirtual <- getVirtualFile vfs $ filePathToUri' file
        pure $ Rope.toText . _text <$> mbVirtual
    case res of
        Left err       -> return ([err], Nothing)
        Right contents -> return ([], Just (time, contents))

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

fileStoreRules :: VFSHandle -> (NormalizedFilePath -> Action Bool) -> Rules ()
fileStoreRules vfs isWatched = do
    addIdeGlobal vfs
    getModificationTimeRule vfs isWatched
    getFileContentsRule vfs
    isFileOfInterestRule

-- | Note that some buffer for a specific file has been modified but not
-- with what changes.
setFileModified :: IdeState
                -> Bool -- ^ Was the file saved?
                -> NormalizedFilePath
                -> IO ()
setFileModified state saved nfp = do
    ideOptions <- getIdeOptionsIO $ shakeExtras state
    doCheckParents <- optCheckParents ideOptions
    let checkParents = case doCheckParents of
          AlwaysCheck         -> True
          CheckOnSaveAndClose -> saved
          _                   -> False
    VFSHandle{..} <- getIdeGlobalState state
    when (isJust setVirtualFileContents) $
        fail "setFileModified can't be called on this type of VFSHandle"
    shakeRestart state []
    when checkParents $
      typecheckParents state nfp

typecheckParents :: IdeState -> NormalizedFilePath -> IO ()
typecheckParents state nfp = void $ shakeEnqueue (shakeExtras state) parents
  where parents = mkDelayedAction "ParentTC" L.Debug (typecheckParentsAction nfp)

typecheckParentsAction :: NormalizedFilePath -> Action ()
typecheckParentsAction nfp = do
    revs <- transitiveReverseDependencies nfp <$> useNoFile_ GetModuleGraph
    logger <- logger <$> getShakeExtras
    let log = L.logInfo logger . T.pack
    case revs of
      Nothing -> liftIO $ log $ "Could not identify reverse dependencies for " ++ show nfp
      Just rs -> do
        liftIO $ (log $ "Typechecking reverse dependencies for " ++ show nfp ++ ": " ++ show revs)
          `catch` \(e :: SomeException) -> log (show e)
        () <$ uses GetModIface rs

-- | Note that some buffer somewhere has been modified, but don't say what.
--   Only valid if the virtual file system was initialised by LSP, as that
--   independently tracks which files are modified.
setSomethingModified :: IdeState -> IO ()
setSomethingModified state = do
    VFSHandle{..} <- getIdeGlobalState state
    when (isJust setVirtualFileContents) $
        fail "setSomethingModified can't be called on this type of VFSHandle"
    -- Update database to remove any files that might have been renamed/deleted
    atomically $ writeTQueue (indexQueue $ hiedbWriter $ shakeExtras state) deleteMissingRealFiles
    void $ shakeRestart state []
