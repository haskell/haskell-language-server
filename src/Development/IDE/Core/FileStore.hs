-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Core.FileStore(
    getFileExists, getFileContents,
    setBufferModified,
    fileStoreRules,
    VFSHandle(..),
    makeVFSHandle,
    makeLSPVFSHandle,
    ) where

import           StringBuffer
import Development.IDE.GHC.Orphans()

import Control.Concurrent.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import           Data.Time.Clock
import           Control.Monad.Extra
import qualified System.Directory as Dir
import           Development.Shake
import           Development.Shake.Classes
import           Development.IDE.Core.Shake
import           Control.Exception
import           GHC.Generics
import Data.Either.Extra
import System.IO.Error
import qualified Data.ByteString.Char8 as BS
import qualified StringBuffer as SB
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import qualified Data.Rope.UTF16 as Rope
import           Data.Time

import Language.Haskell.LSP.Core
import Language.Haskell.LSP.VFS

-- | haskell-lsp manages the VFS internally and automatically so we cannot use
-- the builtin VFS without spawning up an LSP server. To be able to test things
-- like `setBufferModified` we abstract over the VFS implementation.
data VFSHandle = VFSHandle
    { getVirtualFile :: NormalizedUri -> IO (Maybe VirtualFile)
    , setVirtualFileContents :: NormalizedUri -> T.Text -> IO ()
    , removeVirtualFile :: NormalizedUri -> IO ()
    }

instance IsIdeGlobal VFSHandle

makeVFSHandle :: IO VFSHandle
makeVFSHandle = do
    vfsVar <- newVar (1, Map.empty)
    pure VFSHandle
        { getVirtualFile = \uri -> do
              (_nextVersion, vfs) <- readVar vfsVar
              pure $ Map.lookup uri vfs
        , setVirtualFileContents = \uri content ->
              modifyVar_ vfsVar $ \(nextVersion, vfs) ->
                  pure (nextVersion + 1, Map.insert uri (VirtualFile nextVersion (Rope.fromText content) Nothing) vfs)
        , removeVirtualFile = \uri -> modifyVar_ vfsVar $ \(nextVersion, vfs) -> pure (nextVersion, Map.delete uri vfs)
        }

makeLSPVFSHandle :: LspFuncs c -> VFSHandle
makeLSPVFSHandle lspFuncs = VFSHandle
    { getVirtualFile = getVirtualFileFunc lspFuncs
    , setVirtualFileContents = \_ _ -> pure ()
    -- ^ Handled internally by haskell-lsp.
    , removeVirtualFile = \_ -> pure ()
    -- ^ Handled internally by haskell-lsp.
    }


-- | Get the contents of a file, either dirty (if the buffer is modified) or from disk.
type instance RuleResult GetFileContents = (FileVersion, StringBuffer)

-- | Does the file exist.
type instance RuleResult GetFileExists = Bool


data GetFileExists = GetFileExists
    deriving (Eq, Show, Generic)
instance Hashable GetFileExists
instance NFData   GetFileExists

data GetFileContents = GetFileContents
    deriving (Eq, Show, Generic)
instance Hashable GetFileContents
instance NFData   GetFileContents


getFileExistsRule :: VFSHandle -> Rules ()
getFileExistsRule vfs =
    defineEarlyCutoff $ \GetFileExists file -> do
        alwaysRerun
        res <- liftIO $ handle (\(_ :: IOException) -> return False) $
            (isJust <$> getVirtualFile vfs (filePathToUri' file)) ||^
            Dir.doesFileExist (fromNormalizedFilePath file)
        return (Just $ if res then BS.singleton '1' else BS.empty, ([], Just res))


showTimePrecise :: UTCTime -> String
showTimePrecise UTCTime{..} = show (toModifiedJulianDay utctDay, diffTimeToPicoseconds utctDayTime)

getModificationTimeRule :: VFSHandle -> Rules ()
getModificationTimeRule vfs =
    defineEarlyCutoff $ \GetModificationTime file -> do
        let file' = fromNormalizedFilePath file
        let wrap time = (Just $ BS.pack $ showTimePrecise time, ([], Just $ ModificationTime time))
        alwaysRerun
        mbVirtual <- liftIO $ getVirtualFile vfs $ filePathToUri' file
        case mbVirtual of
            Just (VirtualFile ver _ _) -> pure (Just $ BS.pack $ show ver, ([], Just $ VFSVersion ver))
            Nothing -> liftIO $ fmap wrap (Dir.getModificationTime file')
              `catch` \(e :: IOException) -> do
                let err | isDoesNotExistError e = "File does not exist: " ++ file'
                        | otherwise = "IO error while reading " ++ file' ++ ", " ++ displayException e
                return (Nothing, ([ideErrorText file $ T.pack err], Nothing))


getFileContentsRule :: VFSHandle -> Rules ()
getFileContentsRule vfs =
    define $ \GetFileContents file -> do
        -- need to depend on modification time to introduce a dependency with Cutoff
        time <- use_ GetModificationTime file
        res <- liftIO $ ideTryIOException file $ do
            mbVirtual <- getVirtualFile vfs $ filePathToUri' file
            case mbVirtual of
                Just (VirtualFile _ rope _) -> return $ textToStringBuffer $ Rope.toText rope
                Nothing -> hGetStringBuffer (fromNormalizedFilePath file)
        case res of
            Left err -> return ([err], Nothing)
            Right contents -> return ([], Just (time, contents))

ideTryIOException :: NormalizedFilePath -> IO a -> IO (Either FileDiagnostic a)
ideTryIOException fp act =
  mapLeft
      (\(e :: IOException) -> ideErrorText fp $ T.pack $ show e)
      <$> try act


getFileContents :: NormalizedFilePath -> Action (FileVersion, StringBuffer)
getFileContents = use_ GetFileContents

getFileExists :: NormalizedFilePath -> Action Bool
getFileExists =
    -- we deliberately and intentionally wrap the file as an FilePath WITHOUT mkAbsolute
    -- so that if the file doesn't exist, is on a shared drive that is unmounted etc we get a properly
    -- cached 'No' rather than an exception in the wrong place
    use_ GetFileExists


fileStoreRules :: VFSHandle -> Rules ()
fileStoreRules vfs = do
    addIdeGlobal vfs
    getModificationTimeRule vfs
    getFileContentsRule vfs
    getFileExistsRule vfs


-- | Notify the compiler service of a modified buffer
setBufferModified :: IdeState -> NormalizedFilePath -> Maybe T.Text -> IO ()
setBufferModified state absFile mbContents = do
    VFSHandle{..} <- getIdeGlobalState state
    case mbContents of
        Nothing -> removeVirtualFile (filePathToUri' absFile)
        Just contents -> setVirtualFileContents (filePathToUri' absFile) contents
    void $ shakeRun state [] (const $ pure ())


-- would be nice to do this more efficiently...
textToStringBuffer :: T.Text -> SB.StringBuffer
textToStringBuffer = SB.stringToStringBuffer . T.unpack
