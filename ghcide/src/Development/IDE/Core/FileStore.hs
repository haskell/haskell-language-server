-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
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
    ) where

import Development.IDE.GHC.Orphans()
import           Development.IDE.Core.Shake
import Control.Concurrent.Extra
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import           Control.Monad.Extra
import           Development.Shake
import           Development.Shake.Classes
import           Control.Exception
import           GHC.Generics
import Data.Either.Extra
import Data.Int (Int64)
import Data.Time
import System.IO.Error
import qualified Data.ByteString.Char8 as BS
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.Core.OfInterest (getFilesOfInterest)
import Development.IDE.Core.RuleTypes
import Development.IDE.Types.Options
import qualified Data.Rope.UTF16 as Rope
import Development.IDE.Import.DependencyInformation

#ifdef mingw32_HOST_OS
import qualified System.Directory as Dir
#else
import Data.Time.Clock.System (systemToUTCTime, SystemTime(MkSystemTime))
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal (alloca)
import Foreign.Storable
import qualified System.Posix.Error as Posix
#endif

import qualified Development.IDE.Types.Logger as L

import Language.Haskell.LSP.Core
import Language.Haskell.LSP.VFS

-- | haskell-lsp manages the VFS internally and automatically so we cannot use
-- the builtin VFS without spawning up an LSP server. To be able to test things
-- like `setBufferModified` we abstract over the VFS implementation.
data VFSHandle = VFSHandle
    { getVirtualFile :: NormalizedUri -> IO (Maybe VirtualFile)
        -- ^ get the contents of a virtual file
    , setVirtualFileContents :: Maybe (NormalizedUri -> Maybe T.Text -> IO ())
        -- ^ set a specific file to a value. If Nothing then we are ignoring these
        --   signals anyway so can just say something was modified
    }

instance IsIdeGlobal VFSHandle

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

makeLSPVFSHandle :: LspFuncs c -> VFSHandle
makeLSPVFSHandle lspFuncs = VFSHandle
    { getVirtualFile = getVirtualFileFunc lspFuncs
    , setVirtualFileContents = Nothing
   }


isFileOfInterestRule :: Rules ()
isFileOfInterestRule = defineEarlyCutoff $ \IsFileOfInterest f -> do
    filesOfInterest <- getFilesOfInterest
    let res = maybe NotFOI IsFOI $ f `HM.lookup` filesOfInterest
    return (Just $ BS.pack $ show $ hash res, ([], Just res))

-- | Get the contents of a file, either dirty (if the buffer is modified) or Nothing to mean use from disk.
type instance RuleResult GetFileContents = (FileVersion, Maybe T.Text)

data GetFileContents = GetFileContents
    deriving (Eq, Show, Generic)
instance Hashable GetFileContents
instance NFData   GetFileContents
instance Binary   GetFileContents

getModificationTimeRule :: VFSHandle -> Rules ()
getModificationTimeRule vfs =
    defineEarlyCutoff $ \(GetModificationTime_ missingFileDiags) file -> do
        let file' = fromNormalizedFilePath file
        let wrap time@(l,s) = (Just $ BS.pack $ show time, ([], Just $ ModificationTime l s))
        alwaysRerun
        mbVirtual <- liftIO $ getVirtualFile vfs $ filePathToUri' file
        case mbVirtual of
            Just (virtualFileVersion -> ver) ->
                pure (Just $ BS.pack $ show ver, ([], Just $ VFSVersion ver))
            Nothing -> liftIO $ fmap wrap (getModTime file')
              `catch` \(e :: IOException) -> do
                let err | isDoesNotExistError e = "File does not exist: " ++ file'
                        | otherwise = "IO error while reading " ++ file' ++ ", " ++ displayException e
                    diag = ideErrorText file (T.pack err)
                if isDoesNotExistError e && not missingFileDiags
                    then return (Nothing, ([], Nothing))
                    else return (Nothing, ([diag], Nothing))

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
            Left err -> return ([err], Nothing)
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
          IsFOI Modified -> getCurrentTime
          _ -> do
            (large,small) <- getModTime $ fromNormalizedFilePath f
            pure $ internalTimeToUTCTime large small
    return (modTime, txt)

fileStoreRules :: VFSHandle -> Rules ()
fileStoreRules vfs = do
    addIdeGlobal vfs
    getModificationTimeRule vfs
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
    let checkParents = case optCheckParents ideOptions of
          AlwaysCheck -> True
          CheckOnSaveAndClose -> saved
          _ -> False
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
    void $ shakeRestart state []
