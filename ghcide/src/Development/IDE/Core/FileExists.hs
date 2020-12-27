{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.FileExists
  ( fileExistsRules
  , modifyFileExists
  , getFileExists
  , watchedGlobs
  )
where

import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad.Extra
import           Data.Binary
import qualified Data.ByteString               as BS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           Development.Shake
import           Development.Shake.Classes
import           GHC.Generics
import           Language.Haskell.LSP.Types.Capabilities
import qualified System.Directory as Dir
import qualified System.FilePath.Glob as Glob

{- Note [File existence cache and LSP file watchers]
Some LSP servers provide the ability to register file watches with the client, which will then notify
us of file changes. Some clients can do this more efficiently than us, or generally it's a tricky
problem

Here we use this to maintain a quick lookup cache of file existence. How this works is:
- On startup, if the client supports it we ask it to watch some files (see below).
- When those files are created or deleted (we can also see change events, but we don't
care since we're only caching existence here) we get a notification from the client.
- The notification handler calls 'modifyFileExists' to update our cache.

This means that the cache will only ever work for the files we have set up a watcher for.
So we pick the set that we mostly care about and which are likely to change existence
most often: the source files of the project (as determined by the source extensions
we're configured to care about).

For all other files we fall back to the slow path.

There are a few failure modes to think about:

1. The client doesn't send us the notifications we asked for.

There's not much we can do in this case: the whole point is to rely on the client so
we don't do the checking ourselves. If the client lets us down, we will just be wrong.

2. Races between registering watchers, getting notifications, and file changes.

If a file changes status between us asking for notifications and the client actually
setting up the notifications, we might not get told about it. But this is a relatively
small race window around startup, so we just don't worry about it.

3. Using the fast path for files that we aren't watching.

In this case we will fall back to the slow path, but cache that result forever (since
it won't get invalidated by a client notification). To prevent this we guard the
fast path by a check that the path also matches our watching patterns.
-}

-- See Note [File existence cache and LSP file watchers]
-- | A map for tracking the file existence.
-- If a path maps to 'True' then it exists; if it maps to 'False' then it doesn't exist'; and
-- if it's not in the map then we don't know.
type FileExistsMap = (HashMap NormalizedFilePath Bool)

-- | A wrapper around a mutable 'FileExistsState'
newtype FileExistsMapVar = FileExistsMapVar (Var FileExistsMap)

instance IsIdeGlobal FileExistsMapVar

-- | Grab the current global value of 'FileExistsMap' without acquiring a dependency
getFileExistsMapUntracked :: Action FileExistsMap
getFileExistsMapUntracked = do
  FileExistsMapVar v <- getIdeGlobalAction
  liftIO $ readVar v

-- | Modify the global store of file exists.
modifyFileExists :: IdeState -> [(NormalizedFilePath, Bool)] -> IO ()
modifyFileExists state changes = do
  FileExistsMapVar var <- getIdeGlobalState state
  changesMap           <- evaluate $ HashMap.fromList changes
  -- Masked to ensure that the previous values are flushed together with the map update
  mask $ \_ -> do
    -- update the map
    modifyVar_ var $ evaluate . HashMap.union changesMap
    -- See Note [Invalidating file existence results]
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

{- Note [Which files should we watch?]
The watcher system gives us a lot of flexibility: we can set multiple watchers, and they can all watch on glob
patterns.

We used to have a quite precise system, where we would register a watcher for a single file path only (and always)
when we actually looked to see if it existed. The downside of this is that it sends a *lot* of notifications
to the client (thousands on a large project), and this could lock up some clients like emacs
(https://github.com/emacs-lsp/lsp-mode/issues/2165).

Now we take the opposite approach: we register a single, quite general watcher that looks for all files
with a predefined set of extensions. The consequences are:
- The client will have to watch more files. This is usually not too bad, since the pattern is a single glob,
and the clients typically call out to an optimized implementation of file watching that understands globs.
- The client will send us a lot more notifications. This isn't too bad in practice, since although
we're watching a lot of files in principle, they don't get created or destroyed that often.
- We won't ever hit the fast lookup path for files which aren't in our watch pattern, since the only way
files get into our map is when the client sends us a notification about them because we're watching them.
This is fine so long as we're watching the files we check most often, i.e. source files.
-}

-- | The list of file globs that we ask the client to watch.
watchedGlobs :: IdeOptions -> [String]
watchedGlobs opts = [ "**/*." ++ extIncBoot | ext <- optExtensions opts, extIncBoot <- [ext, ext ++ "-boot"]]

-- | Installs the 'getFileExists' rules.
--   Provides a fast implementation if client supports dynamic watched files.
--   Creates a global state as a side effect in that case.
fileExistsRules :: ClientCapabilities -> VFSHandle -> Rules ()
fileExistsRules ClientCapabilities{_workspace} vfs = do
  -- Create the global always, although it should only be used if we have fast rules.
  -- But there's a chance someone will send unexpected notifications anyway,
  -- e.g. https://github.com/haskell/ghcide/issues/599
  addIdeGlobal . FileExistsMapVar =<< liftIO (newVar [])

  extras <- getShakeExtrasRules
  opts <- liftIO $ getIdeOptionsIO extras
  let globs = watchedGlobs opts

  case () of
    _ | Just WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
      , Just DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
      , Just True <- _dynamicRegistration
        -> fileExistsRulesFast globs vfs
      | otherwise -> fileExistsRulesSlow vfs

-- Requires an lsp client that provides WatchedFiles notifications, but assumes that this has already been checked.
fileExistsRulesFast :: [String] -> VFSHandle -> Rules ()
fileExistsRulesFast globs vfs =
    let patterns = fmap Glob.compile globs
        fpMatches fp = any (\p -> Glob.match p fp) patterns
    in defineEarlyCutoff $ \GetFileExists file -> do
        isWf <- isWorkspaceFile file
        if isWf && fpMatches (fromNormalizedFilePath file)
            then fileExistsFast vfs file
            else fileExistsSlow vfs file

{- Note [Invalidating file existence results]
We have two mechanisms for getting file existence information:
- The file existence cache
- The VFS lookup

Both of these affect the results of the 'GetFileExists' rule, so we need to make sure it
is invalidated properly when things change.

For the file existence cache, we manually flush the results of 'GetFileExists' when we
modify it (i.e. when a notification comes from the client). This is faster than using
'alwaysRerun' in the 'fileExistsFast', and we need it to be as fast as possible.

For the VFS lookup, however, we won't get prompted to flush the result, so instead
we use 'alwaysRerun'.
-}

fileExistsFast :: VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsFast vfs file = do
    -- Could in principle use 'alwaysRerun' here, but it's too slwo, See Note [Invalidating file existence results]
    mp <- getFileExistsMapUntracked

    let mbFilesWatched = HashMap.lookup file mp
    exist <- case mbFilesWatched of
      Just exist -> pure exist
      -- We don't know about it: use the slow route.
      -- Note that we do *not* call 'fileExistsSlow', as that would trigger 'alwaysRerun'.
      Nothing -> liftIO $ getFileExistsVFS vfs file
    pure (summarizeExists exist, ([], Just exist))

summarizeExists :: Bool -> Maybe BS.ByteString
summarizeExists x = Just $ if x then BS.singleton 1 else BS.empty

fileExistsRulesSlow :: VFSHandle -> Rules ()
fileExistsRulesSlow vfs =
  defineEarlyCutoff $ \GetFileExists file -> fileExistsSlow vfs file

fileExistsSlow :: VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsSlow vfs file = do
    -- See Note [Invalidating file existence results]
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
