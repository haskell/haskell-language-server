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
    isFileOfInterestRule
    ,modifyFileStore) where

import           Control.Concurrent.Extra
import           Control.Concurrent.STM                       (atomically)
import           Control.Concurrent.STM.TQueue                (writeTQueue)
import           Control.Exception
import           Control.Monad.Extra
import qualified Data.ByteString.Char8                        as BS
import           Data.Either.Extra
import qualified Data.HashMap.Strict                          as HM
import           Data.Int                                     (Int64)
import qualified Data.Map.Strict                              as Map
import           Data.Maybe
import qualified Data.Rope.UTF16                              as Rope
import qualified Data.Text                                    as T
import           Data.Time
import           Development.IDE.Core.OfInterest              (getFilesOfInterest)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Orphans                  ()
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           Development.Shake
import           Development.Shake.Classes
import           HieDb.Create                                 (deleteMissingRealFiles)
import           Ide.Plugin.Config                            (CheckParents (..))
import           System.IO.Error

#ifdef mingw32_HOST_OS
import qualified System.Directory                             as Dir
#else
import           Data.Time.Clock.System                       (SystemTime (MkSystemTime),
                                                               systemToUTCTime)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal                              (alloca)
import           Foreign.Ptr
import           Foreign.Storable
import qualified System.Posix.Error                           as Posix
#endif

import qualified Development.IDE.Types.Logger                 as L

import           Language.LSP.Server                          hiding
                                                              (getVirtualFile)
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.Types                           (FileChangeType (FcChanged),
                                                               FileEvent (FileEvent),
                                                               uriToFilePath)
import           Language.LSP.VFS

makeVFSHandle :: IO VFSHandle
makeVFSHandle = do
    vfsVar <- newVar (1, Map.empty)
    pure VFSHandle
        { getVirtualFile = \uri -> do
              (_nextVersion, vfs) <- readVar vfsVar
              pure $ Map.lookup uri vfs
        , setVirtualFileContents = Just $ \uri content ->
              modifyVar_ vfsVar $ \(nextVersion, vfs) -> pure $ (nextVersion + 1, ) $
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
isFileOfInterestRule = defineEarlyCutoff $ \IsFileOfInterest f -> do
    filesOfInterest <- getFilesOfInterest
    let res = maybe NotFOI IsFOI $ f `HM.lookup` filesOfInterest
    return (Just $ BS.pack $ show $ hash res, ([], Just res))

getModificationTimeRule :: VFSHandle -> (NormalizedFilePath -> Action Bool) -> Rules ()
getModificationTimeRule vfs isWatched =
    defineEarlyCutoff $ \(GetModificationTime_ missingFileDiags) file -> do
        let file' = fromNormalizedFilePath file
        let wrap time@(l,s) = (Just $ BS.pack $ show time, ([], Just $ ModificationTime l s))
        mbVirtual <- liftIO $ getVirtualFile vfs $ filePathToUri' file
        case mbVirtual of
            Just (virtualFileVersion -> ver) -> do
                alwaysRerun
                pure (Just $ BS.pack $ show ver, ([], Just $ VFSVersion ver))
            Nothing -> do
                isWF <- isWatched file
                unless isWF alwaysRerun
                liftIO $ fmap wrap (getModTime file')
                    `catch` \(e :: IOException) -> do
                        let err | isDoesNotExistError e = "File does not exist: " ++ file'
                                | otherwise = "IO error while reading " ++ file' ++ ", " ++ displayException e
                            diag = ideErrorText file (T.pack err)
                        if isDoesNotExistError e && not missingFileDiags
                            then return (Nothing, ([], Nothing))
                            else return (Nothing, ([diag], Nothing))

-- | Reset the GetModificationTime state of watched files
modifyFileStore :: IdeState -> [FileEvent] -> IO ()
modifyFileStore state changes = mask $ \_ ->
    forM_ changes $ \(FileEvent uri c) ->
        case c of
            FcChanged
              | Just f <- uriToFilePath uri
              -> do
                  deleteValue state (GetModificationTime_ True) (toNormalizedFilePath' f)
                  deleteValue state (GetModificationTime_ False) (toNormalizedFilePath' f)
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
getModTime :: FilePath -> IO (Int64, Int64)
getModTime f =
#ifdef mingw32_HOST_OS
    do time <- Dir.getModificationTime f
       let !day = fromInteger $ toModifiedJulianDay $ utctDay time
           !dayTime = fromInteger $ diffTimeToPicoseconds $ utctDayTime time
       pure (day, dayTime)
#else
    withCString f $ \f' ->
    alloca $ \secPtr ->
    alloca $ \nsecPtr -> do
        Posix.throwErrnoPathIfMinus1Retry_ "getmodtime" f $ c_getModTime f' secPtr nsecPtr
        CTime sec <- peek secPtr
        CLong nsec <- peek nsecPtr
        pure (sec, nsec)

-- Sadly even unix’s getFileStatus + modificationTimeHiRes is still about twice as slow
-- as doing the FFI call ourselves :(.
foreign import ccall "getmodtime" c_getModTime :: CString -> Ptr CTime -> Ptr CLong -> IO Int
#endif

modificationTime :: FileVersion -> Maybe UTCTime
modificationTime VFSVersion{} = Nothing
modificationTime (ModificationTime large small) = Just $ internalTimeToUTCTime large small

internalTimeToUTCTime :: Int64 -> Int64 -> UTCTime
internalTimeToUTCTime large small =
#ifdef mingw32_HOST_OS
    UTCTime (ModifiedJulianDay $ fromIntegral large) (picosecondsToDiffTime $ fromIntegral small)
#else
    systemToUTCTime $ MkSystemTime large (fromIntegral small)
#endif

getFileContentsRule :: VFSHandle -> Rules ()
getFileContentsRule vfs =
    define $ \GetFileContents file -> do
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
            (large,small) <- getModTime $ fromNormalizedFilePath f
            pure $ internalTimeToUTCTime large small
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
