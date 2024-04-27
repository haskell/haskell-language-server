{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.FileExists
  ( fileExistsRules
  , modifyFileExists
  , getFileExists
  , watchedGlobs
  , GetFileExists(..)
  , Log(..)
  )
where

import           Control.Concurrent.STM.Stats          (atomically,
                                                        atomicallyNamed)
import           Control.Exception
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.ByteString                       as BS
import           Data.List                             (partition)
import           Data.Maybe
import           Development.IDE.Core.FileStore        hiding (Log, LogShake)
import qualified Development.IDE.Core.FileStore        as FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake            hiding (Log)
import qualified Development.IDE.Core.Shake            as Shake
import           Development.IDE.Graph
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           Development.IDE.Types.Shake           (toKey)
import qualified Focus
import           Ide.Logger                            (Pretty (pretty),
                                                        Recorder, WithPriority,
                                                        cmapWithPrio)
import           Ide.Plugin.Config                     (Config)
import           Language.LSP.Protocol.Types
import           Language.LSP.Server                   hiding (getVirtualFile)
import qualified StmContainers.Map                     as STM
import qualified System.Directory                      as Dir
import qualified System.FilePath.Glob                  as Glob

{- Note [File existence cache and LSP file watchers]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
type FileExistsMap = STM.Map NormalizedFilePath Bool

-- | A wrapper around a mutable 'FileExistsState'
newtype FileExistsMapVar = FileExistsMapVar FileExistsMap

instance IsIdeGlobal FileExistsMapVar

data Log
  = LogFileStore FileStore.Log
  | LogShake Shake.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogFileStore msg -> pretty msg
    LogShake msg     -> pretty msg

-- | Grab the current global value of 'FileExistsMap' without acquiring a dependency
getFileExistsMapUntracked :: Action FileExistsMap
getFileExistsMapUntracked = do
  FileExistsMapVar v <- getIdeGlobalAction
  return v

-- | Modify the global store of file exists.
modifyFileExists :: IdeState -> [(NormalizedFilePath, FileChangeType)] -> IO [Key]
modifyFileExists state changes = do
  FileExistsMapVar var <- getIdeGlobalState state
  -- Masked to ensure that the previous values are flushed together with the map update
  mask_ $ atomicallyNamed "modifyFileExists" $ do
    forM_ changes $ \(f,c) ->
        case fromChange c of
            Just c' -> STM.focus (Focus.insert c') f var
            Nothing -> pure ()
    -- See Note [Invalidating file existence results]
    -- flush previous values
    let (fileModifChanges, fileExistChanges) =
            partition ((== FileChangeType_Changed) . snd) changes
    keys0 <- concat <$> mapM (deleteValue (shakeExtras state) GetFileExists . fst) fileExistChanges
    let keys1 = map (toKey GetFileExists . fst) fileExistChanges
    let keys2 = map (toKey GetModificationTime . fst) fileModifChanges
    return (keys0 <> keys1 <> keys2)

fromChange :: FileChangeType -> Maybe Bool
fromChange FileChangeType_Created = Just True
fromChange FileChangeType_Deleted = Just False
fromChange FileChangeType_Changed = Nothing

-------------------------------------------------------------------------------------

-- | Returns True if the file exists
getFileExists :: NormalizedFilePath -> Action Bool
getFileExists fp = use_ GetFileExists fp

{- Note [Which files should we watch?]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
watchedGlobs opts = [ "**/*." ++ ext | ext <- allExtensions opts]

allExtensions :: IdeOptions -> [String]
allExtensions opts = [extIncBoot | ext <- optExtensions opts, extIncBoot <- [ext, ext ++ "-boot"]]

-- | Installs the 'getFileExists' rules.
--   Provides a fast implementation if client supports dynamic watched files.
--   Creates a global state as a side effect in that case.
fileExistsRules :: Recorder (WithPriority Log) -> Maybe (LanguageContextEnv Config) -> Rules ()
fileExistsRules recorder lspEnv = do
  supportsWatchedFiles <- case lspEnv of
    Nothing      -> pure False
    Just lspEnv' -> liftIO $  runLspT lspEnv' isWatchSupported
  -- Create the global always, although it should only be used if we have fast rules.
  -- But there's a chance someone will send unexpected notifications anyway,
  -- e.g. https://github.com/haskell/ghcide/issues/599
  addIdeGlobal . FileExistsMapVar =<< liftIO STM.newIO

  extras <- getShakeExtrasRules
  opts <- liftIO $ getIdeOptionsIO extras
  let globs = watchedGlobs opts
      patterns = fmap Glob.compile globs
      fpMatches fp = any (`Glob.match`fp) patterns
      isWatched = if supportsWatchedFiles
        then \f -> do
            isWF <- isWorkspaceFile f
            return $ isWF && fpMatches (fromNormalizedFilePath f)
        else const $ pure False

  if supportsWatchedFiles
    then fileExistsRulesFast recorder isWatched
    else fileExistsRulesSlow recorder

  fileStoreRules (cmapWithPrio LogFileStore recorder) isWatched

-- Requires an lsp client that provides WatchedFiles notifications, but assumes that this has already been checked.
fileExistsRulesFast :: Recorder (WithPriority Log) -> (NormalizedFilePath -> Action Bool) -> Rules ()
fileExistsRulesFast recorder isWatched =
    defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \GetFileExists file -> do
        isWF <- isWatched file
        if isWF
            then fileExistsFast file
            else fileExistsSlow file

{- Note [Invalidating file existence results]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

fileExistsFast :: NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe Bool)
fileExistsFast file = do
    -- Could in principle use 'alwaysRerun' here, but it's too slwo, See Note [Invalidating file existence results]
    mp <- getFileExistsMapUntracked

    mbFilesWatched <- liftIO $ atomically $ STM.lookup file mp
    exist <- case mbFilesWatched of
      Just exist -> pure exist
      -- We don't know about it: use the slow route.
      -- Note that we do *not* call 'fileExistsSlow', as that would trigger 'alwaysRerun'.
      Nothing    -> getFileExistsVFS file
    pure (summarizeExists exist, Just exist)

summarizeExists :: Bool -> Maybe BS.ByteString
summarizeExists x = Just $ if x then BS.singleton 1 else BS.empty

fileExistsRulesSlow :: Recorder (WithPriority Log) -> Rules ()
fileExistsRulesSlow recorder =
  defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \GetFileExists file -> fileExistsSlow file

fileExistsSlow :: NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe Bool)
fileExistsSlow file = do
    -- See Note [Invalidating file existence results]
    alwaysRerun
    exist <- getFileExistsVFS file
    pure (summarizeExists exist, Just exist)

getFileExistsVFS :: NormalizedFilePath -> Action Bool
getFileExistsVFS file = do
  vf <- getVirtualFile file
  if isJust vf
  then pure True
  else liftIO $ handle (\(_ :: IOException) -> return False) $
         Dir.doesFileExist (fromNormalizedFilePath file)
