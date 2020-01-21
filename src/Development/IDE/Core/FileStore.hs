-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Core.FileStore(
    getFileContents,
    getVirtualFile,
    setBufferModified,
    setSomethingModified,
    fileStoreRules,
    VFSHandle,
    makeVFSHandle,
    makeLSPVFSHandle,
    getSourceFingerprint
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Fingerprint
import           StringBuffer
import Development.IDE.GHC.Orphans()
import Development.IDE.GHC.Util
import           Development.IDE.Core.Shake
import Control.Concurrent.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import           Control.Monad.Extra
import           Development.Shake
import           Development.Shake.Classes
import           Control.Exception
import           GHC.Generics
import Data.Either.Extra
import System.IO.Error
import qualified Data.ByteString.Char8 as BS
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import qualified Data.Rope.UTF16 as Rope

#ifdef mingw32_HOST_OS
import Data.Time
#else
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal (alloca)
import Foreign.Storable
import qualified System.Posix.Error as Posix
#endif

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


-- | Get the contents of a file, either dirty (if the buffer is modified) or Nothing to mean use from disk.
type instance RuleResult GetFileContents = (FileVersion, Maybe StringBuffer)

type instance RuleResult FingerprintSource = Fingerprint

data GetFileContents = GetFileContents
    deriving (Eq, Show, Generic)
instance Hashable GetFileContents
instance NFData   GetFileContents
instance Binary   GetFileContents

data FingerprintSource = FingerprintSource
    deriving (Eq, Show, Generic)
instance Hashable FingerprintSource
instance NFData   FingerprintSource
instance Binary   FingerprintSource

fingerprintSourceRule :: Rules ()
fingerprintSourceRule =
    define $ \FingerprintSource file -> do
      (_, mbContent) <- getFileContents file
      content <- liftIO $ maybe (hGetStringBuffer $ fromNormalizedFilePath file) pure mbContent
      fingerprint <- liftIO $ fpStringBuffer content
      pure ([], Just fingerprint)
    where fpStringBuffer (StringBuffer buf len cur) = withForeignPtr buf $ \ptr -> fingerprintData (ptr `plusPtr` cur) len

getModificationTimeRule :: VFSHandle -> Rules ()
getModificationTimeRule vfs =
    defineEarlyCutoff $ \GetModificationTime file -> do
        let file' = fromNormalizedFilePath file
        let wrap time = (Just time, ([], Just $ ModificationTime time))
        alwaysRerun
        mbVirtual <- liftIO $ getVirtualFile vfs $ filePathToUri' file
        case mbVirtual of
            Just (virtualFileVersion -> ver) -> pure (Just $ BS.pack $ show ver, ([], Just $ VFSVersion ver))
            Nothing -> liftIO $ fmap wrap (getModTime file')
              `catch` \(e :: IOException) -> do
                let err | isDoesNotExistError e = "File does not exist: " ++ file'
                        | otherwise = "IO error while reading " ++ file' ++ ", " ++ displayException e
                return (Nothing, ([ideErrorText file $ T.pack err], Nothing))
  where
    -- Dir.getModificationTime is surprisingly slow since it performs
    -- a ton of conversions. Since we do not actually care about
    -- the format of the time, we can get away with something cheaper.
    -- For now, we only try to do this on Unix systems where it seems to get the
    -- time spent checking file modifications (which happens on every change)
    -- from > 0.5s to ~0.15s.
    -- We might also want to try speeding this up on Windows at some point.
    -- TODO leverage DidChangeWatchedFile lsp notifications on clients that
    -- support them, as done for GetFileExists
    getModTime :: FilePath -> IO BS.ByteString
    getModTime f =
#ifdef mingw32_HOST_OS
        do time <- Dir.getModificationTime f
           pure $! BS.pack $ show (toModifiedJulianDay $ utctDay time, diffTimeToPicoseconds $ utctDayTime time)
#else
        withCString f $ \f' ->
        alloca $ \secPtr ->
        alloca $ \nsecPtr -> do
            Posix.throwErrnoPathIfMinus1Retry_ "getmodtime" f $ c_getModTime f' secPtr nsecPtr
            sec <- peek secPtr
            nsec <- peek nsecPtr
            pure $! BS.pack $ show sec <> "." <> show nsec

-- Sadly even unix’s getFileStatus + modificationTimeHiRes is still about twice as slow
-- as doing the FFI call ourselves :(.
foreign import ccall "getmodtime" c_getModTime :: CString -> Ptr CTime -> Ptr CLong -> IO Int
#endif

getSourceFingerprint :: NormalizedFilePath -> Action Fingerprint
getSourceFingerprint = use_ FingerprintSource

getFileContentsRule :: VFSHandle -> Rules ()
getFileContentsRule vfs =
    define $ \GetFileContents file -> do
        -- need to depend on modification time to introduce a dependency with Cutoff
        time <- use_ GetModificationTime file
        res <- liftIO $ ideTryIOException file $ do
            mbVirtual <- getVirtualFile vfs $ filePathToUri' file
            pure $ textToStringBuffer . Rope.toText . _text <$> mbVirtual
        case res of
            Left err -> return ([err], Nothing)
            Right contents -> return ([], Just (time, contents))

ideTryIOException :: NormalizedFilePath -> IO a -> IO (Either FileDiagnostic a)
ideTryIOException fp act =
  mapLeft
      (\(e :: IOException) -> ideErrorText fp $ T.pack $ show e)
      <$> try act


getFileContents :: NormalizedFilePath -> Action (FileVersion, Maybe StringBuffer)
getFileContents = use_ GetFileContents

fileStoreRules :: VFSHandle -> Rules ()
fileStoreRules vfs = do
    addIdeGlobal vfs
    getModificationTimeRule vfs
    getFileContentsRule vfs
    fingerprintSourceRule


-- | Notify the compiler service that a particular file has been modified.
--   Use 'Nothing' to say the file is no longer in the virtual file system
--   but should be sourced from disk, or 'Just' to give its new value.
setBufferModified :: IdeState -> NormalizedFilePath -> Maybe T.Text -> IO ()
setBufferModified state absFile contents = do
    VFSHandle{..} <- getIdeGlobalState state
    whenJust setVirtualFileContents $ \set ->
        set (filePathToUri' absFile) contents
    void $ shakeRun state []

-- | Note that some buffer somewhere has been modified, but don't say what.
--   Only valid if the virtual file system was initialised by LSP, as that
--   independently tracks which files are modified.
setSomethingModified :: IdeState -> IO ()
setSomethingModified state = do
    VFSHandle{..} <- getIdeGlobalState state
    when (isJust setVirtualFileContents) $
        fail "setSomethingModified can't be called on this type of VFSHandle"
    void $ shakeRun state []
