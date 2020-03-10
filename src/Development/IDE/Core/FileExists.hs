{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.FileExists
  ( fileExistsRules
  , modifyFileExists
  , getFileExists
  )
where

import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad.Extra
import qualified Data.Aeson                    as A
import           Data.Binary
import qualified Data.ByteString               as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import qualified Data.Text                     as T
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.Shake
import           Development.Shake.Classes
import           GHC.Generics
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities
import qualified System.Directory as Dir

-- | A map for tracking the file existence
type FileExistsMap = (HashMap NormalizedFilePath Bool)

-- | A wrapper around a mutable 'FileExistsMap'
newtype FileExistsMapVar = FileExistsMapVar (Var FileExistsMap)

instance IsIdeGlobal FileExistsMapVar

-- | Grab the current global value of 'FileExistsMap' without acquiring a dependency
getFileExistsMapUntracked :: Action FileExistsMap
getFileExistsMapUntracked = do
  FileExistsMapVar v <- getIdeGlobalAction
  liftIO $ readVar v

-- | Modify the global store of file exists
modifyFileExistsAction :: (FileExistsMap -> IO FileExistsMap) -> Action ()
modifyFileExistsAction f = do
  FileExistsMapVar var <- getIdeGlobalAction
  liftIO $ modifyVar_ var f

-- | Modify the global store of file exists
modifyFileExists :: IdeState -> [(NormalizedFilePath, Bool)] -> IO ()
modifyFileExists state changes = do
  FileExistsMapVar var <- getIdeGlobalState state
  changesMap           <- evaluate $ HashMap.fromList changes

  -- Masked to ensure that the previous values are flushed together with the map update
  mask $ \_ -> do
    -- update the map
    modifyVar_ var $ evaluate . HashMap.union changesMap
    -- flush previous values
    mapM_ (deleteValue state GetFileExists . fst) changes

-------------------------------------------------------------------------------------

type instance RuleResult GetFileExists = Bool

data GetFileExists = GetFileExists
    deriving (Eq, Show, Typeable, Generic)

instance NFData   GetFileExists
instance Hashable GetFileExists
instance Binary   GetFileExists

-- | Returns True if the file exists
--   Note that a file is not considered to exist unless it is saved to disk.
--   In particular, VFS existence is not enough.
--   Consider the following example:
--     1. The file @A.hs@ containing the line @import B@ is added to the files of interest
--        Since @B.hs@ is neither open nor exists, GetLocatedImports finds Nothing
--     2. The editor creates a new buffer @B.hs@
--        Unless the editor also sends a @DidChangeWatchedFile@ event, ghcide will not pick it up
--        Most editors, e.g. VSCode, only send the event when the file is saved to disk.
getFileExists :: NormalizedFilePath -> Action Bool
getFileExists fp = use_ GetFileExists fp

-- | Installs the 'getFileExists' rules.
--   Provides a fast implementation if client supports dynamic watched files.
--   Creates a global state as a side effect in that case.
fileExistsRules :: IO LspId -> ClientCapabilities -> VFSHandle -> Rules ()
fileExistsRules getLspId ClientCapabilities{_workspace} vfs
  | Just WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
  , Just DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
  , Just True <- _dynamicRegistration
  = fileExistsRulesFast getLspId vfs
  | otherwise = do
      logger <- logger <$> getShakeExtrasRules
      liftIO $ logDebug logger "Warning: Client does not support watched files. Falling back to OS polling"
      fileExistsRulesSlow vfs

--   Requires an lsp client that provides WatchedFiles notifications.
fileExistsRulesFast :: IO LspId -> VFSHandle -> Rules ()
fileExistsRulesFast getLspId vfs = do
  addIdeGlobal . FileExistsMapVar =<< liftIO (newVar [])
  defineEarlyCutoff $ \GetFileExists file -> do
    isWf <- isWorkspaceFile file
    if isWf
        then fileExistsFast getLspId vfs file
        else fileExistsSlow vfs file

fileExistsFast :: IO LspId -> VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsFast getLspId vfs file = do
    fileExistsMap <- getFileExistsMapUntracked
    let mbFilesWatched = HashMap.lookup file fileExistsMap
    case mbFilesWatched of
      Just fv -> pure (summarizeExists fv, ([], Just fv))
      Nothing -> do
        exist                   <- liftIO $ getFileExistsVFS vfs file
        ShakeExtras { eventer } <- getShakeExtras

        -- add a listener for VFS Create/Delete file events,
        -- taking the FileExistsMap lock to prevent race conditions
        -- that would lead to multiple listeners for the same path
        modifyFileExistsAction $ \x -> do
          case HashMap.alterF (,Just exist) file x of
            (Nothing, x') -> do
            -- if the listener addition fails, we never recover. This is a bug.
              addListener eventer file
              return x'
            (Just _, _) ->
              -- if the key was already there, do nothing
              return x

        pure (summarizeExists exist, ([], Just exist))
 where
  addListener eventer fp = do
    reqId <- getLspId
    let
      req = RequestMessage "2.0" reqId ClientRegisterCapability regParams
      fpAsId       = T.pack $ fromNormalizedFilePath fp
      regParams    = RegistrationParams (List [registration])
      registration = Registration fpAsId
                                  WorkspaceDidChangeWatchedFiles
                                  (Just (A.toJSON regOptions))
      regOptions =
        DidChangeWatchedFilesRegistrationOptions { watchers = List [watcher] }
      watcher = FileSystemWatcher { globPattern = fromNormalizedFilePath fp
                                  , kind        = Just 5 -- Create and Delete events only
                                  }

    eventer $ ReqRegisterCapability req

summarizeExists :: Bool -> Maybe BS.ByteString
summarizeExists x = Just $ if x then BS.singleton 1 else BS.empty

fileExistsRulesSlow:: VFSHandle -> Rules ()
fileExistsRulesSlow vfs =
  defineEarlyCutoff $ \GetFileExists file -> fileExistsSlow vfs file

fileExistsSlow :: VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsSlow vfs file = do
    alwaysRerun
    exist <- liftIO $ getFileExistsVFS vfs file
    pure (summarizeExists exist, ([], Just exist))

getFileExistsVFS :: VFSHandle -> NormalizedFilePath -> IO Bool
getFileExistsVFS vfs file = do
    -- we deliberately and intentionally wrap the file as an FilePath WITHOUT mkAbsolute
    -- so that if the file doesn't exist, is on a shared drive that is unmounted etc we get a properly
    -- cached 'No' rather than an exception in the wrong place
    handle (\(_ :: IOException) -> return False) $
        (isJust <$> getVirtualFile vfs (filePathToUri' file)) ||^
        Dir.doesFileExist (fromNormalizedFilePath file)

--------------------------------------------------------------------------------------------------
-- The message definitions below probably belong in haskell-lsp-types

data DidChangeWatchedFilesRegistrationOptions = DidChangeWatchedFilesRegistrationOptions
    { watchers :: List FileSystemWatcher
    }

instance A.ToJSON DidChangeWatchedFilesRegistrationOptions where
  toJSON DidChangeWatchedFilesRegistrationOptions {..} =
    A.object ["watchers" A..= watchers]

data FileSystemWatcher = FileSystemWatcher
    { -- | The glob pattern to watch.
      --   For details on glob pattern syntax, check the spec: https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#workspace_didChangeWatchedFiles
      globPattern :: String
        -- | The kind of event to subscribe to. Defaults to all.
        --   Defined as a bitmap of Create(1), Change(2), and Delete(4)
    , kind        :: Maybe Int
    }

instance A.ToJSON FileSystemWatcher where
  toJSON FileSystemWatcher {..} =
    A.object
      $  ["globPattern" A..= globPattern]
      ++ [ "kind" A..= x | Just x <- [kind] ]
