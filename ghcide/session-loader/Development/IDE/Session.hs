{-# LANGUAGE TypeFamilies #-}

{-|
The logic for setting up a ghcide session by tapping into hie-bios.
-}
module Development.IDE.Session
  (SessionLoadingOptions(..)
  ,CacheDirs(..)
  ,loadSessionWithOptions
  ,getInitialGhcLibDirDefault
  ,getHieDbLoc
  ,retryOnSqliteBusy
  ,retryOnException
  ,Log(..)
  ,runWithDb
  ) where

-- Unfortunately, we cannot use loadSession with ghc-lib since hie-bios uses
-- the real GHC library and the types are incompatible. Furthermore, when
-- building with ghc-lib we need to make this Haskell agnostic, so no hie-bios!

import           Control.Concurrent.Strict
import           Control.Exception.Safe              as Safe
import           Control.Monad
import           Control.Monad.Extra                 as Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe           (MaybeT (MaybeT, runMaybeT))
import qualified Crypto.Hash.SHA1                    as H
import           Data.Aeson                          hiding (Error, Key)
import qualified Data.ByteString.Base16              as B16
import qualified Data.ByteString.Char8               as B
import           Data.Default
import           Data.Hashable                       hiding (hash)
import qualified Data.HashMap.Strict                 as HM
import           Data.List
import           Data.List.Extra                     as L
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                           as T
import           Data.Version
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake          hiding (Log, knownTargets,
                                                      withHieDb)
import qualified Development.IDE.GHC.Compat          as Compat
import           Development.IDE.GHC.Compat.Core     hiding (Target, TargetFile,
                                                      TargetModule, Var,
                                                      Warning, getOptions)
import           Development.IDE.GHC.Compat.Env      hiding (Logger)
import           Development.IDE.GHC.Util
import           Development.IDE.Graph               (Action, Key)
import qualified Development.IDE.Session.Implicit    as GhcIde
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq      (HscEnvEq)
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import qualified HIE.Bios                            as HieBios
import           HIE.Bios.Environment                hiding (getCacheDir)
import           HIE.Bios.Types                      hiding (Log)
import qualified HIE.Bios.Types                      as HieBios
import           Ide.Logger                          (Pretty (pretty),
                                                      Priority (Debug, Error, Info, Warning),
                                                      Recorder, WithPriority,
                                                      cmapWithPrio, logWith,
                                                      nest,
                                                      toCologActionWithPrio,
                                                      vcat, viaShow, (<+>))
import           Ide.Types                           (Config,
                                                      SessionLoadingPreferenceConfig (..),
                                                      sessionLoading)
import           Language.LSP.Protocol.Message
import           Language.LSP.Server
import           System.Directory
import qualified System.Directory.Extra              as IO
import           System.FilePath
import           System.Info

import           Control.Applicative                 (Alternative ((<|>)))
import           Data.Void

import           Control.Concurrent.STM.Stats        (atomically, modifyTVar',
                                                      readTVar, writeTVar)
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Trans.Cont            (ContT (ContT, runContT))
import           Data.Foldable                       (for_)
import           Data.HashMap.Strict                 (HashMap)
import           Data.HashSet                        (HashSet)
import qualified Data.HashSet                        as Set
import           Database.SQLite.Simple
import           Development.IDE.Core.Tracing        (withTrace)
import           Development.IDE.Core.WorkerThread   (withWorkerQueue)
import           Development.IDE.Session.Dependency
import           Development.IDE.Session.Diagnostics (renderCradleError)
import           Development.IDE.Session.Ghc         hiding (Log)
import           Development.IDE.Types.Shake         (WithHieDb,
                                                      WithHieDbShield (..),
                                                      toNoFileKey)
import           HieDb.Create
import           HieDb.Types
import           Ide.PluginUtils                     (toAbsolute)
import qualified System.Random                       as Random
import           System.Random                       (RandomGen)
import           Text.ParserCombinators.ReadP        (readP_to_S)

import           Control.Concurrent.STM              (STM, TVar)
import qualified Control.Monad.STM                   as STM
import           Control.Monad.Trans.Reader
import qualified Development.IDE.Session.Ghc         as Ghc
import qualified Development.IDE.Session.OrderedSet  as S
import qualified Focus
import qualified StmContainers.Map                   as STM

data Log
  = LogSettingInitialDynFlags
  | LogGetInitialGhcLibDirDefaultCradleFail !CradleError !FilePath !(Maybe FilePath) !(Cradle Void)
  | LogGetInitialGhcLibDirDefaultCradleNone
  | LogHieDbRetry !Int !Int !Int !SomeException
  | LogHieDbRetriesExhausted !Int !Int !Int !SomeException
  | LogHieDbWriterThreadSQLiteError !SQLError
  | LogHieDbWriterThreadException !SomeException
  | LogKnownFilesUpdated !(HashMap Target (HashSet NormalizedFilePath))
  | LogCradlePath !FilePath
  | LogCradleNotFound !FilePath
  | LogSessionLoadingResult !(Either [CradleError] (ComponentOptions, FilePath, String))
  | LogCradle !(Cradle Void)
  | LogNoneCradleFound FilePath
  | LogHieBios HieBios.Log
  | LogSessionLoadingChanged
  | LogSessionNewLoadedFiles ![FilePath]
  | LogSessionReloadOnError FilePath ![FilePath]
  | LogGetOptionsLoop !FilePath
  | LogLookupSessionCache !FilePath
  | LogTime !String
  | LogSessionGhc Ghc.Log
deriving instance Show Log

instance Pretty Log where
  pretty = \case
    LogTime s -> "Time:" <+> pretty s
    LogLookupSessionCache path -> "Looking up session cache for" <+> pretty path
    LogGetOptionsLoop fp -> "Loop: getOptions for" <+> pretty fp
    LogSessionReloadOnError path files ->
      "Reloading file due to error in" <+> pretty path <+> "with files:" <+> pretty files
    LogSessionNewLoadedFiles files ->
      "New loaded files:" <+> pretty files
    LogNoneCradleFound path ->
      "None cradle found for" <+> pretty path <+> ", ignoring the file"
    LogSettingInitialDynFlags ->
      "Setting initial dynflags..."
    LogGetInitialGhcLibDirDefaultCradleFail cradleError rootDirPath hieYamlPath cradle ->
      nest 2 $
        vcat
          [ "Couldn't load cradle for ghc libdir."
          , "Cradle error:" <+> viaShow cradleError
          , "Root dir path:" <+> pretty rootDirPath
          , "hie.yaml path:" <+> pretty hieYamlPath
          , "Cradle:" <+> viaShow cradle ]
    LogGetInitialGhcLibDirDefaultCradleNone ->
      "Couldn't load cradle. Cradle not found."
    LogHieDbRetry delay maxDelay retriesRemaining e ->
      nest 2 $
        vcat
          [ "Retrying hiedb action..."
          , "delay:" <+> pretty delay
          , "maximum delay:" <+> pretty maxDelay
          , "retries remaining:" <+> pretty retriesRemaining
          , "SQLite error:" <+> pretty (displayException e) ]
    LogHieDbRetriesExhausted baseDelay maxDelay retriesRemaining e ->
      nest 2 $
        vcat
          [ "Retries exhausted for hiedb action."
          , "base delay:" <+> pretty baseDelay
          , "maximum delay:" <+> pretty maxDelay
          , "retries remaining:" <+> pretty retriesRemaining
          , "Exception:" <+> pretty (displayException e) ]
    LogHieDbWriterThreadSQLiteError e ->
      nest 2 $
        vcat
          [ "HieDb writer thread SQLite error:"
          , pretty (displayException e) ]
    LogHieDbWriterThreadException e ->
      nest 2 $
        vcat
          [ "HieDb writer thread exception:"
          , pretty (displayException e) ]
    LogKnownFilesUpdated targetToPathsMap ->
      nest 2 $
        vcat
          [ "Known files updated:"
          , viaShow $ (HM.map . Set.map) fromNormalizedFilePath targetToPathsMap
          ]
    LogCradlePath path ->
      "Cradle path:" <+> pretty path
    LogCradleNotFound path ->
      vcat
        [ "No [cradle](https://github.com/mpickering/hie-bios#hie-bios) found for" <+> pretty path <> "."
        , "Proceeding with [implicit cradle](https://hackage.haskell.org/package/implicit-hie)."
        , "You should ignore this message, unless you see a 'Multi Cradle: No prefixes matched' error." ]
    LogSessionLoadingResult e ->
      "Session loading result:" <+> viaShow e
    LogCradle cradle ->
      "Cradle:" <+> viaShow cradle
    LogHieBios msg -> pretty msg
    LogSessionGhc msg -> pretty msg
    LogSessionLoadingChanged ->
      "Session Loading config changed, reloading the full session."

-- | Bump this version number when making changes to the format of the data stored in hiedb
hiedbDataVersion :: String
hiedbDataVersion = "2"

data SessionLoadingOptions = SessionLoadingOptions
  { findCradle             :: FilePath -> IO (Maybe FilePath)
  -- | Load the cradle with an optional 'hie.yaml' location.
  -- If a 'hie.yaml' is given, use it to load the cradle.
  -- Otherwise, use the provided project root directory to determine the cradle type.
  , loadCradle             :: Recorder (WithPriority Log) -> Maybe FilePath -> FilePath -> IO (HieBios.Cradle Void)
  -- | Given the project name and a set of command line flags,
  --   return the path for storing generated GHC artifacts,
  --   or 'Nothing' to respect the cradle setting
  , getCacheDirs           :: String -> [String] -> IO CacheDirs
  -- | Return the GHC lib dir to use for the 'unsafeGlobalDynFlags'
  , getInitialGhcLibDir    :: Recorder (WithPriority Log) -> FilePath -> IO (Maybe LibDir)
  }

instance Default SessionLoadingOptions where
    def =  SessionLoadingOptions
        {findCradle = HieBios.findCradle
        ,loadCradle = loadWithImplicitCradle
        ,getCacheDirs = getCacheDirsDefault
        ,getInitialGhcLibDir = getInitialGhcLibDirDefault
        }

-- | Find the cradle for a given 'hie.yaml' configuration.
--
-- If a 'hie.yaml' is given, the cradle is read from the config.
--  If this config does not comply to the "hie.yaml"
-- specification, an error is raised.
--
-- If no location for "hie.yaml" is provided, the implicit config is used
-- using the provided root directory for discovering the project.
-- The implicit config uses different heuristics to determine the type
-- of the project that may or may not be accurate.
loadWithImplicitCradle
  :: Recorder (WithPriority Log)
  -> Maybe FilePath
  -- ^ Optional 'hie.yaml' location. Will be used if given.
  -> FilePath
  -- ^ Root directory of the project. Required as a fallback
  -- if no 'hie.yaml' location is given.
  -> IO (HieBios.Cradle Void)
loadWithImplicitCradle recorder mHieYaml rootDir = do
  let logger = toCologActionWithPrio (cmapWithPrio LogHieBios recorder)
  case mHieYaml of
    Just yaml -> HieBios.loadCradle logger yaml
    Nothing   -> GhcIde.loadImplicitCradle logger rootDir

getInitialGhcLibDirDefault :: Recorder (WithPriority Log) -> FilePath -> IO (Maybe LibDir)
getInitialGhcLibDirDefault recorder rootDir = do
  hieYaml <- findCradle def (rootDir </> "a")
  cradle <- loadCradle def recorder hieYaml rootDir
  libDirRes <- getRuntimeGhcLibDir cradle
  case libDirRes of
      CradleSuccess libdir -> pure $ Just $ LibDir libdir
      CradleFail err -> do
        logWith recorder Error $ LogGetInitialGhcLibDirDefaultCradleFail err rootDir hieYaml cradle
        pure Nothing
      CradleNone -> do
        logWith recorder Warning LogGetInitialGhcLibDirDefaultCradleNone
        pure Nothing

-- | If the action throws exception that satisfies predicate then we sleep for
-- a duration determined by the random exponential backoff formula,
-- `uniformRandom(0, min (maxDelay, (baseDelay * 2) ^ retryAttempt))`, and try
-- the action again for a maximum of `maxRetryCount` times.
-- `MonadIO`, `MonadCatch` are used as constraints because there are a few
-- HieDb functions that don't return IO values.
retryOnException
  :: (MonadIO m, MonadCatch m, RandomGen g, Exception e)
  => (e -> Maybe e) -- ^ only retry on exception if this predicate returns Just
  -> Recorder (WithPriority Log)
  -> Int -- ^ maximum backoff delay in microseconds
  -> Int -- ^ base backoff delay in microseconds
  -> Int -- ^ maximum number of times to retry
  -> g -- ^ random number generator
  -> m a -- ^ action that may throw exception
  -> m a
retryOnException exceptionPred recorder maxDelay !baseDelay !maxTimesRetry rng action = do
  result <- tryJust exceptionPred action
  case result of
    Left e
      | maxTimesRetry > 0 -> do
        -- multiply by 2 because baseDelay is midpoint of uniform range
        let newBaseDelay = min maxDelay (baseDelay * 2)
        let (delay, newRng) = Random.randomR (0, newBaseDelay) rng
        let newMaxTimesRetry = maxTimesRetry - 1
        liftIO $ do
          logWith recorder Warning $ LogHieDbRetry delay maxDelay newMaxTimesRetry (toException e)
          threadDelay delay
        retryOnException exceptionPred recorder maxDelay newBaseDelay newMaxTimesRetry newRng action

      | otherwise -> do
        liftIO $ do
          logWith recorder Warning $ LogHieDbRetriesExhausted baseDelay maxDelay maxTimesRetry (toException e)
          throwIO e

    Right b -> pure b

-- | in microseconds
oneSecond :: Int
oneSecond = 1000000

-- | in microseconds
oneMillisecond :: Int
oneMillisecond = 1000

-- | default maximum number of times to retry hiedb call
maxRetryCount :: Int
maxRetryCount = 10

retryOnSqliteBusy :: (MonadIO m, MonadCatch m, RandomGen g)
                  => Recorder (WithPriority Log) -> g -> m a -> m a
retryOnSqliteBusy recorder rng action =
  let isErrorBusy e
        | SQLError{ sqlError = ErrorBusy } <- e = Just e
        | otherwise = Nothing
  in
    retryOnException isErrorBusy recorder oneSecond oneMillisecond maxRetryCount rng action

makeWithHieDbRetryable :: RandomGen g => Recorder (WithPriority Log) -> g -> HieDb -> WithHieDb
makeWithHieDbRetryable recorder rng hieDb f =
  retryOnSqliteBusy recorder rng (f hieDb)

-- | Wraps `withHieDb` to provide a database connection for reading, and a `HieWriterChan` for
-- writing. Actions are picked off one by one from the `HieWriterChan` and executed in serial
-- by a worker thread using a dedicated database connection.
-- This is done in order to serialize writes to the database, or else SQLite becomes unhappy
--
-- Also see Note [Serializing runs in separate thread]
runWithDb :: Recorder (WithPriority Log) -> FilePath -> ContT () IO (WithHieDbShield, IndexQueue)
runWithDb recorder fp = ContT $ \k -> do
  -- use non-deterministic seed because maybe multiple HLS start at same time
  -- and send bursts of requests
  rng <- Random.newStdGen
  -- Delete the database if it has an incompatible schema version
  retryOnSqliteBusy
    recorder
    rng
    (withHieDb fp (const $ pure ()) `Safe.catch` \IncompatibleSchemaVersion{} -> removeFile fp)

  withHieDb fp $ \writedb -> do
    -- the type signature is necessary to avoid concretizing the tyvar
    -- e.g. `withWriteDbRetryable initConn` without type signature will
    -- instantiate tyvar `a` to `()`
    let withWriteDbRetryable :: WithHieDb
        withWriteDbRetryable = makeWithHieDbRetryable recorder rng writedb
    withWriteDbRetryable initConn


    -- Clear the index of any files that might have been deleted since the last run
    _ <- withWriteDbRetryable deleteMissingRealFiles
    _ <- withWriteDbRetryable garbageCollectTypeNames

    runContT (withWorkerQueue (writer withWriteDbRetryable)) $ \chan ->
        withHieDb fp (\readDb -> k (WithHieDbShield $ makeWithHieDbRetryable recorder rng readDb, chan))
  where
    writer withHieDbRetryable l = do
        -- TODO: probably should let exceptions be caught/logged/handled by top level handler
        l withHieDbRetryable
          `Safe.catch` \e@SQLError{} -> do
            logWith recorder Error $ LogHieDbWriterThreadSQLiteError e
          `Safe.catchAny` \f -> do
            logWith recorder Error $ LogHieDbWriterThreadException f


getHieDbLoc :: FilePath -> IO FilePath
getHieDbLoc dir = do
  let db = intercalate "-" [dirHash, takeBaseName dir, Compat.ghcVersionStr, hiedbDataVersion] <.> "hiedb"
      dirHash = B.unpack $ B16.encode $ H.hash $ B.pack dir
  cDir <- IO.getXdgDirectory IO.XdgCache cacheDir
  createDirectoryIfMissing True cDir
  pure (cDir </> db)

{- Note [SessionState and batch load]
SessionState manages the state for batch loading files in the session loader.

- When a new file needs to be loaded, it is added to the pendingFiles set.
- The loader processes files from pendingFiles, attempting to load them in batches.
- (SBL1) If a file is already in failedFiles, it is loaded individually (single-file mode).
- (SBL2) Otherwise, the loader tries to load as many files as possible together (batch mode).

On success:
  - (SBL3) All successfully loaded files are removed from pendingFiles and failedFiles,
    and added to loadedFiles.

On failure:
  - (SBL4) If loading a single file fails, it is added to failedFiles and removed from loadedFiles and pendingFiles.
  - (SBL5) If batch loading fails, all files attempted are added to failedFiles.

This approach ensures efficient batch loading while isolating problematic files for individual handling.
-}

-- SBL3
handleBatchLoadSuccess :: Foldable t => Recorder (WithPriority Log) -> SessionState -> Maybe FilePath -> HashMap NormalizedFilePath (IdeResult HscEnvEq, DependencyInfo) -> t TargetDetails -> IO ()
handleBatchLoadSuccess recorder sessionState hieYaml this_flags_map all_targets =  do
  pendings <- getPendingFiles sessionState
  -- this_flags_map might contains files not in pendingFiles, take the intersection
  let newLoaded = pendings `Set.intersection` Set.fromList (fromNormalizedFilePath <$> HM.keys this_flags_map)
  atomically $ do
    STM.insert this_flags_map hieYaml (fileToFlags sessionState)
    insertAllFileMappings sessionState $ map ((hieYaml,) . fst) $ concatMap toFlagsMap all_targets
  logWith recorder Info $ LogSessionNewLoadedFiles $ Set.toList newLoaded
  atomically $ forM_ (Set.toList newLoaded) $ flip S.delete (pendingFiles sessionState)
  mapM_ (removeErrorLoadingFile sessionState) (Set.toList newLoaded)
  addCradleFiles sessionState newLoaded

-- SBL5
handleBatchLoadFailure :: SessionState -> [FilePath] -> IO ()
handleBatchLoadFailure sessionState files = do
  mapM_ (addErrorLoadingFile sessionState) files

-- SBL4
handleSingleLoadFailure :: SessionState -> FilePath -> IO ()
handleSingleLoadFailure sessionState file = do
  addErrorLoadingFile sessionState file
  removeErrorLoadingFile sessionState file
  atomically $ S.delete file (pendingFiles sessionState)
  removeCradleFile sessionState file

data SessionState = SessionState
  { loadedFiles  :: !(Var (HashSet FilePath)),
    failedFiles  :: !(Var (HashSet FilePath)),
    pendingFiles :: !(S.OrderedSet FilePath),
    hscEnvs      :: !(Var HieMap),
    fileToFlags  :: !FlagsMap,
    filesMap     :: !FilesMap,
    version      :: !(Var Int),
    sessionLoadingPreferenceConfig :: !(Var (Maybe SessionLoadingPreferenceConfig))
  }

-- | Helper functions for SessionState management
-- These functions encapsulate common operations on the SessionState

-- | Add a file to the set of files with errors during loading
addErrorLoadingFile :: MonadIO m =>  SessionState -> FilePath -> m ()
addErrorLoadingFile state file =
  liftIO $ modifyVar_' (failedFiles state) (\xs -> return $ Set.insert file xs)

-- | Remove a file from the set of files with errors during loading
removeErrorLoadingFile :: MonadIO m => SessionState -> FilePath -> m ()
removeErrorLoadingFile state file =
  liftIO $ modifyVar_' (failedFiles state) (\xs -> return $ Set.delete file xs)

addCradleFiles :: MonadIO m => SessionState -> HashSet FilePath -> m ()
addCradleFiles state files =
  liftIO $ modifyVar_' (loadedFiles state) (\xs -> return $ files <> xs)

-- | Remove a file from the cradle files set
removeCradleFile :: MonadIO m =>  SessionState -> FilePath -> m ()
removeCradleFile state file =
  liftIO $ modifyVar_' (loadedFiles state) (\xs -> return $ Set.delete file xs)

-- | Clear error loading files and reset to empty set
clearErrorLoadingFiles :: MonadIO m => SessionState -> m ()
clearErrorLoadingFiles state =
  liftIO $ modifyVar_' (failedFiles state) (const $ return Set.empty)

-- | Clear cradle files and reset to empty set
clearCradleFiles :: MonadIO m => SessionState -> m ()
clearCradleFiles state =
  liftIO $ modifyVar_' (loadedFiles state) (const $ return Set.empty)

-- | Reset the file maps in the session state
resetFileMaps :: SessionState -> STM ()
resetFileMaps state = do
  STM.reset (filesMap state)
  STM.reset (fileToFlags state)

-- | Insert or update file flags for a specific hieYaml and normalized file path
insertFileFlags :: SessionState -> Maybe FilePath -> NormalizedFilePath -> (IdeResult HscEnvEq, DependencyInfo) -> STM ()
insertFileFlags state hieYaml ncfp flags =
  STM.focus (Focus.insertOrMerge HM.union (HM.singleton ncfp flags)) hieYaml (fileToFlags state)

-- | Insert a file mapping from normalized path to hieYaml location
insertFileMapping :: SessionState -> Maybe FilePath -> NormalizedFilePath -> STM ()
insertFileMapping state hieYaml ncfp =
  STM.insert hieYaml ncfp (filesMap state)

-- | Remove a file from the pending file set
removeFromPending :: SessionState -> FilePath -> STM ()
removeFromPending state file =
  S.delete file (pendingFiles state)

-- | Add a file to the pending file set
addToPending :: SessionState -> FilePath -> STM ()
addToPending state file =
  S.insert file (pendingFiles state)

-- | Insert multiple file mappings at once
insertAllFileMappings :: SessionState -> [(Maybe FilePath, NormalizedFilePath)] -> STM ()
insertAllFileMappings state mappings =
  mapM_ (\(yaml, path) -> insertFileMapping state yaml path) mappings

-- | Increment the version counter
incrementVersion :: SessionState -> IO Int
incrementVersion state = modifyVar' (version state) succ

-- | Get files from the pending file set
getPendingFiles :: SessionState -> IO (HashSet FilePath)
getPendingFiles state = atomically $ S.toHashSet (pendingFiles state)

-- | Handle errors during session loading by recording file as having error and removing from pending
handleSingleFileProcessingError' :: SessionState -> Maybe FilePath -> FilePath -> PackageSetupException -> SessionM ()
handleSingleFileProcessingError' state hieYaml file e = do
  handleSingleFileProcessingError state hieYaml file [renderPackageSetupException file e] mempty

-- | Common pattern: Insert file flags, insert file mapping, and remove from pending
handleSingleFileProcessingError :: SessionState -> Maybe FilePath -> FilePath -> [FileDiagnostic] -> [FilePath] -> SessionM ()
handleSingleFileProcessingError state hieYaml file diags extraDepFiles = liftIO $ do
  dep <- getDependencyInfo $ maybeToList hieYaml <> extraDepFiles
  let ncfp = toNormalizedFilePath' file
  let flags = ((diags, Nothing), dep)
  handleSingleLoadFailure state file
  atomically $ do
    insertFileFlags state hieYaml ncfp flags
    insertFileMapping state hieYaml ncfp

-- | Get the set of extra files to load based on the current file path
-- If the current file is in error loading files, we fallback to single loading mode (empty set)
-- Otherwise, we remove error files from pending files and also exclude the current file
getExtraFilesToLoad :: SessionState -> FilePath -> IO [FilePath]
getExtraFilesToLoad state cfp = do
  pendingFiles <- getPendingFiles state
  errorFiles <- readVar (failedFiles state)
  old_files <- readVar (loadedFiles state)
  -- if the file is in error loading files, we fall back to single loading mode
  return $
    Set.toList $
      if cfp `Set.member` errorFiles
        then Set.empty
        -- remove error files from pending files since error loading need to load one by one
        else (Set.delete cfp $ pendingFiles `Set.difference` errorFiles) <> old_files

-- | We allow users to specify a loading strategy.
-- Check whether this config was changed since the last time we have loaded
-- a session.
--
-- If the loading configuration changed, we likely should restart the session
-- in its entirety.
didSessionLoadingPreferenceConfigChange :: SessionState -> SessionM Bool
didSessionLoadingPreferenceConfigChange s = do
    clientConfig <- asks sessionClientConfig
    let biosSessionLoadingVar = sessionLoadingPreferenceConfig s
    mLoadingConfig <- liftIO $ readVar biosSessionLoadingVar
    case mLoadingConfig of
        Nothing -> do
            liftIO $ writeVar biosSessionLoadingVar (Just (sessionLoading clientConfig))
            pure False
        Just loadingConfig -> do
            liftIO $ writeVar biosSessionLoadingVar (Just (sessionLoading clientConfig))
            pure (loadingConfig /= sessionLoading clientConfig)

newSessionState :: IO SessionState
newSessionState = do
  -- Initialize SessionState
  sessionState <- SessionState
    <$> newVar (Set.fromList [])  -- loadedFiles
    <*> newVar (Set.fromList [])  -- failedFiles
    <*> S.newIO                     -- pendingFiles
    <*> newVar Map.empty            -- hscEnvs
    <*> STM.newIO                   -- fileToFlags
    <*> STM.newIO                   -- filesMap
    <*> newVar 0                    -- version
    <*> newVar Nothing              -- sessionLoadingPreferenceConfig
  return sessionState

-- | Given a root directory, return a Shake 'Action' which setups an
-- 'IdeGhcSession' given a file.
-- Some of the many things this does:
--
-- * Find the cradle for the file
-- * Get the session options,
-- * Get the GHC lib directory
-- * Make sure the GHC compiletime and runtime versions match
-- * Restart the Shake session
--
-- This is the key function which implements multi-component support. All
-- components mapping to the same hie.yaml file are mapped to the same
-- HscEnv which is updated as new components are discovered.

loadSessionWithOptions :: Recorder (WithPriority Log) -> SessionLoadingOptions -> FilePath -> TQueue (IO ()) -> IO (Action IdeGhcSession)
loadSessionWithOptions recorder SessionLoadingOptions{..} rootDir que = do
  let toAbsolutePath = toAbsolute rootDir -- see Note [Root Directory]

  sessionState <- newSessionState
  let returnWithVersion fun = IdeGhcSession fun <$> liftIO (readVar (version sessionState))

  -- This caches the mapping from Mod.hs -> hie.yaml
  cradleLoc <- liftIO $ memoIO $ \v -> do
      res <- findCradle v
      -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
      -- try and normalise that
      -- e.g. see https://github.com/haskell/ghcide/issues/126
      let res' = toAbsolutePath <$> res
      return $ normalise <$> res'

  return $ do
    clientConfig <- getClientConfigAction
    extras@ShakeExtras{ideNc, knownTargetsVar
                      } <- getShakeExtras
    let invalidateShakeCache = do
            void $ incrementVersion sessionState
            return $ toNoFileKey GhcSessionIO

    ideOptions <- getIdeOptions

    -- see Note [Serializing runs in separate thread]
    -- Start the getOptionsLoop if the queue is empty
    liftIO $ atomically $ Extra.whenM (isEmptyTQueue que) $ do
      let newSessionLoadingOptions = SessionLoadingOptions
            { findCradle = cradleLoc
            , ..
            }
          sessionShake = SessionShake
            { restartSession = restartShakeSession extras
            , invalidateCache = invalidateShakeCache
            , enqueueActions = shakeEnqueue extras
            }
          sessionEnv = SessionEnv
            { sessionLspContext = lspEnv extras
            , sessionRootDir = rootDir
            , sessionIdeOptions = ideOptions
            , sessionClientConfig = clientConfig
            , sessionSharedNameCache = ideNc
            , sessionLoadingOptions = newSessionLoadingOptions
            }

      writeTQueue que (runReaderT (getOptionsLoop recorder sessionShake sessionState knownTargetsVar) sessionEnv)

    -- Each one of deps will be registered as a FileSystemWatcher in the GhcSession action
    -- so that we can get a workspace/didChangeWatchedFiles notification when a dep changes.
    -- The GlobPattern of a FileSystemWatcher can be absolute or relative.
    -- We use the absolute one because it is supported by more LSP clients.
    -- Here we make sure deps are absolute and later we use those absolute deps as GlobPattern.
    let absolutePathsCradleDeps (eq, deps) = (eq, fmap toAbsolutePath $ Map.keys deps)
    returnWithVersion $ \file -> do
      let absFile = toAbsolutePath file
      absolutePathsCradleDeps <$> lookupOrWaitCache recorder sessionState absFile

-- | Given a file, this function will return the HscEnv and the dependencies
-- it would look up the cache first, if the cache is not available, it would
-- submit a request to the getOptionsLoop to get the options for the file
-- and wait until the options are available
lookupOrWaitCache :: Recorder (WithPriority Log) -> SessionState -> FilePath -> IO (IdeResult HscEnvEq, DependencyInfo)
lookupOrWaitCache recorder sessionState absFile = do
  let ncfp = toNormalizedFilePath' absFile
  res <- atomically $ do
      -- wait until target file is not in pendingFiles
      Extra.whenM (S.lookup absFile (pendingFiles sessionState)) STM.retry
      -- check if in the cache
      checkInCache sessionState ncfp
  logWith recorder Debug $ LogLookupSessionCache absFile
  updateDateRes <-  case res of
    Just r -> do
        depOk <- checkDependencyInfo (snd r)
        if depOk
            then return $ Just r
            else return Nothing
    _ -> return Nothing
  case updateDateRes of
    Just r -> return r
    Nothing -> do
      -- if not ok, we need to reload the session
      atomically $ addToPending sessionState absFile
      lookupOrWaitCache recorder sessionState absFile

checkInCache :: SessionState -> NormalizedFilePath -> STM (Maybe (IdeResult HscEnvEq, DependencyInfo))
checkInCache sessionState ncfp = runMaybeT $ do
  cachedHieYamlLocation <- MaybeT $ STM.lookup ncfp (filesMap sessionState)
  m <- MaybeT $ STM.lookup cachedHieYamlLocation (fileToFlags sessionState)
  MaybeT $ pure $ HM.lookup ncfp m

data SessionShake = SessionShake
  { restartSession :: VFSModified -> String -> [DelayedAction ()] -> IO [Key] -> IO ()
  , invalidateCache :: IO Key
  , enqueueActions :: DelayedAction () -> IO (IO ())
  }

data SessionEnv = SessionEnv
  { sessionLspContext      :: Maybe (LanguageContextEnv Config)
  , sessionRootDir         :: FilePath
  , sessionIdeOptions      :: IdeOptions
  , sessionClientConfig    :: Config
  , sessionSharedNameCache :: NameCache
  , sessionLoadingOptions  :: SessionLoadingOptions
  }

type SessionM = ReaderT SessionEnv IO

-- | The main function which gets options for a file.
--
-- The general approach is as follows:
-- 1. Find the 'hie.yaml' for the next file target, if there is any.
-- 2. Check in the cache, whether the given 'hie.yaml' was already loaded before
-- 3.1. If it wasn't, initialise a new session and continue with step 4.
-- 3.2. If it is loaded, check whether we need to reload the session, e.g. because the `.cabal` file was modified
-- 3.2.1. If we need to reload, remove the
getOptionsLoop :: Recorder (WithPriority Log) -> SessionShake -> SessionState -> TVar (Hashed KnownTargets) -> SessionM ()
getOptionsLoop recorder sessionShake sessionState knownTargetsVar = forever $ do
  -- Get the next file to load
  file <- liftIO $ atomically $ S.readQueue (pendingFiles sessionState)
  logWith recorder Debug (LogGetOptionsLoop file)
  let ncfp = toNormalizedFilePath' file
  cachedHieYamlLocation <- join <$> liftIO (atomically (STM.lookup ncfp (filesMap sessionState)))
  sessionLoadingOptions <- asks sessionLoadingOptions
  hieYaml <- liftIO $ findCradle sessionLoadingOptions file
  let hieLoc = cachedHieYamlLocation <|> hieYaml
  sessionOpts recorder sessionShake sessionState knownTargetsVar (hieLoc, file)
    `Safe.catch` handleSingleFileProcessingError' sessionState hieLoc file

-- | This caches the mapping from hie.yaml + Mod.hs -> [String]
-- Returns the Ghc session and the cradle dependencies
sessionOpts :: Recorder (WithPriority Log) -> SessionShake -> SessionState -> TVar (Hashed KnownTargets) -> (Maybe FilePath, FilePath) -> SessionM ()
sessionOpts recorder sessionShake sessionState knownTargetsVar (hieYaml, file) = do
  Extra.whenM (didSessionLoadingPreferenceConfigChange sessionState) $ do
    logWith recorder Info LogSessionLoadingChanged
    liftIO $ atomically $ resetFileMaps sessionState
    -- Don't even keep the name cache, we start from scratch here!
    liftIO $ modifyVar_ (hscEnvs sessionState) (const (return Map.empty))
    -- cleanup error loading files and cradle files
    clearErrorLoadingFiles sessionState
    clearCradleFiles sessionState
    cacheKey <- liftIO $ invalidateCache sessionShake
    liftIO $ restartSession sessionShake VFSUnmodified "didSessionLoadingPreferenceConfigChange" [] (return [cacheKey])

  v <- liftIO $ atomically $ STM.lookup hieYaml (fileToFlags sessionState)
  case v >>= HM.lookup (toNormalizedFilePath' file) of
    Just (_opts, old_di) -> do
      deps_ok <- liftIO $ checkDependencyInfo old_di
      if not deps_ok
        then do
          -- if deps are old, we can try to load the error files again
          removeErrorLoadingFile sessionState file
          removeCradleFile sessionState file
          -- If the dependencies are out of date then clear both caches and start
          -- again.
          liftIO $ atomically $ resetFileMaps sessionState
          -- Keep the same name cache
          liftIO $ modifyVar_ (hscEnvs sessionState) (return . Map.adjust (const []) hieYaml)
          consultCradle recorder sessionShake sessionState knownTargetsVar hieYaml file
        else do
          -- if deps are ok, we can just remove the file from pending files
          liftIO $ atomically $ removeFromPending sessionState file
    Nothing ->
        consultCradle recorder sessionShake sessionState knownTargetsVar hieYaml file

consultCradle :: Recorder (WithPriority Log) -> SessionShake -> SessionState -> TVar (Hashed KnownTargets) -> Maybe FilePath -> FilePath -> SessionM ()
consultCradle recorder sessionShake sessionState knownTargetsVar hieYaml cfp = do
  loadingOptions <- asks sessionLoadingOptions
  (cradle, eopts) <- loadCradleWithNotifications recorder
    sessionState
    (loadCradle loadingOptions recorder)
    hieYaml cfp
  logWith recorder Debug $ LogSessionLoadingResult eopts
  let ncfp = toNormalizedFilePath' cfp
  case eopts of
    -- The cradle gave us some options so get to work turning them
    -- into and HscEnv.
    Right (opts, libDir, version) -> do
      let compileTime = fullCompilerVersion
      case reverse $ readP_to_S parseVersion version of
        [] -> error $ "GHC version could not be parsed: " <> version
        ((runTime, _):_)
          | compileTime == runTime -> session recorder sessionShake sessionState knownTargetsVar (hieYaml, ncfp, opts, libDir)
          | otherwise -> handleSingleFileProcessingError' sessionState hieYaml cfp (GhcVersionMismatch{..})
    -- Failure case, either a cradle error or the none cradle
    Left err -> do
        -- what if the error to load file is one of old_files ?
        let attemptToLoadFiles = Set.delete cfp $ Set.fromList $ concatMap cradleErrorLoadingFiles err
        old_files <- liftIO $ readVar (loadedFiles sessionState)
        let errorToLoadNewFiles = cfp : Set.toList (attemptToLoadFiles `Set.difference` old_files)
        if length errorToLoadNewFiles > 1
        then do
            -- we are loading more files and failed, we need to retry
            -- mark as less loaded files as failedLoadingFiles as possible
            -- limitation is that when we are loading files, and the dependencies of old_files
            -- are changed, and old_files are not valid anymore.
            -- but they will still be in the old_files, and will not move to failedFiles.
            -- And make other files failed to load in batch mode.
            liftIO $ handleBatchLoadFailure sessionState errorToLoadNewFiles
            -- retry without other files
            logWith recorder Info $ LogSessionReloadOnError cfp (Set.toList attemptToLoadFiles)
            consultCradle recorder sessionShake sessionState knownTargetsVar hieYaml cfp
        else do
            -- we are only loading this file and it failed
            let res = map (\err' -> renderCradleError err' cradle ncfp) err
            handleSingleFileProcessingError sessionState hieYaml cfp res $ concatMap cradleErrorDependencies err

session ::
    Recorder (WithPriority Log) ->
    SessionShake ->
    SessionState ->
    TVar (Hashed KnownTargets) ->
    (Maybe FilePath, NormalizedFilePath, ComponentOptions, FilePath) ->
    SessionM ()
session recorder sessionShake sessionState knownTargetsVar(hieYaml, cfp, opts, libDir) = do
  let initEmptyHscEnv = emptyHscEnvM libDir
  (new_deps, old_deps) <- packageSetup recorder sessionState initEmptyHscEnv (hieYaml, cfp, opts)

  -- For each component, now make a new HscEnvEq which contains the
  -- HscEnv for the hie.yaml file but the DynFlags for that component
  -- For GHC's supporting multi component sessions, we create a shared
  -- HscEnv but set the active component accordingly
  hscEnv <- initEmptyHscEnv
  ideOptions <- asks sessionIdeOptions
  let new_cache = newComponentCache (cmapWithPrio LogSessionGhc recorder) (optExtensions ideOptions) cfp hscEnv
  all_target_details <- liftIO $ new_cache old_deps new_deps
  (all_targets, this_flags_map) <- liftIO $ addErrorTargetIfUnknown all_target_details hieYaml cfp
  -- The VFS doesn't change on cradle edits, re-use the old one.
  -- Invalidate all the existing GhcSession build nodes by restarting the Shake session
  liftIO $ do
    checkProject <- optCheckProject ideOptions
    restartSession sessionShake VFSUnmodified "new component" [] $ do
        -- It is necessary to call handleBatchLoadSuccess in restartSession
        -- to ensure the GhcSession rule does not return before a new session is started.
        -- Otherwise, invalid compilation results may propagate to downstream rules,
        -- potentially resulting in lost diagnostics and other issues.
        handleBatchLoadSuccess recorder sessionState hieYaml this_flags_map all_targets
        keys2 <- invalidateCache sessionShake
        keys1 <- extendKnownTargets recorder knownTargetsVar all_targets
        -- Typecheck all files in the project on startup
        unless (null new_deps || not checkProject) $ do
            cfps' <- liftIO $ filterM (IO.doesFileExist . fromNormalizedFilePath) (concatMap targetLocations all_targets)
            void $ enqueueActions sessionShake $ mkDelayedAction "InitialLoad" Debug $ void $ do
                mmt <- uses GetModificationTime cfps'
                let cs_exist = catMaybes (zipWith (<$) cfps' mmt)
                modIfaces <- uses GetModIface cs_exist
                -- update exports map
                shakeExtras <- getShakeExtras
                let !exportsMap' = createExportsMap $ mapMaybe (fmap hirModIface) modIfaces
                liftIO $ atomically $ modifyTVar' (exportsMap shakeExtras) (exportsMap' <>)
        return [keys1, keys2]

-- | Create a new HscEnv from a hieYaml root and a set of options
packageSetup :: Recorder (WithPriority Log) -> SessionState -> SessionM HscEnv -> (Maybe FilePath, NormalizedFilePath, ComponentOptions) -> SessionM ([ComponentInfo], [ComponentInfo])
packageSetup recorder sessionState newEmptyHscEnv (hieYaml, cfp, opts) = do
  getCacheDirs <- asks (getCacheDirs . sessionLoadingOptions)
  haddockparse <- asks (optHaddockParse . sessionIdeOptions)
  rootDir <- asks sessionRootDir
  -- Parse DynFlags for the newly discovered component
  hscEnv <- newEmptyHscEnv
  newTargetDfs <- liftIO $ evalGhcEnv hscEnv $ setOptions haddockparse cfp opts (hsc_dflags hscEnv) rootDir
  let deps = componentDependencies opts ++ maybeToList hieYaml
  dep_info <- liftIO $ getDependencyInfo (fmap (toAbsolute rootDir) deps)
  -- Now lookup to see whether we are combining with an existing HscEnv
  -- or making a new one. The lookup returns the HscEnv and a list of
  -- information about other components loaded into the HscEnv
  -- (unitId, DynFlag, Targets)
  liftIO $ modifyVar (hscEnvs sessionState) $
    addComponentInfo (cmapWithPrio LogSessionGhc recorder) getCacheDirs dep_info newTargetDfs (hieYaml, cfp, opts)

addErrorTargetIfUnknown :: Foldable t => t [TargetDetails] -> Maybe FilePath -> NormalizedFilePath -> IO ([TargetDetails], HashMap NormalizedFilePath (IdeResult HscEnvEq, DependencyInfo))
addErrorTargetIfUnknown all_target_details hieYaml cfp = do
  let flags_map' = HM.fromList (concatMap toFlagsMap all_targets')
      all_targets' = concat all_target_details
  this_dep_info <- getDependencyInfo $ maybeToList hieYaml
  let (all_targets, this_flags_map) = case HM.lookup cfp flags_map' of
        Just _ -> (all_targets', flags_map')
        Nothing -> (this_target_details : all_targets', HM.insert cfp this_flags flags_map')
          where
                this_target_details = TargetDetails (TargetFile cfp) this_error_env this_dep_info [cfp]
                this_flags = (this_error_env, this_dep_info)
                this_error_env = ([this_error], Nothing)
                this_error = ideErrorWithSource (Just "cradle") (Just DiagnosticSeverity_Error) cfp
                                (T.unlines
                                  [ "No cradle target found. Is this file listed in the targets of your cradle?"
                                  , "If you are using a .cabal file, please ensure that this module is listed in either the exposed-modules or other-modules section"
                                  ])
                                Nothing
  pure (all_targets, this_flags_map)

-- | Populate the knownTargetsVar with all the
-- files in the project so that `knownFiles` can learn about them and
-- we can generate a complete module graph
extendKnownTargets :: Recorder (WithPriority Log) -> TVar (Hashed KnownTargets) -> [TargetDetails] -> IO Key
extendKnownTargets recorder knownTargetsVar newTargets = do
  knownTargets <- concatForM  newTargets $ \TargetDetails{..} ->
    case targetTarget of
      TargetFile f -> do
        -- If a target file has multiple possible locations, then we
        -- assume they are all separate file targets.
        -- This happens with '.hs-boot' files if they are in the root directory of the project.
        -- GHC reports options such as '-i. A' as 'TargetFile A.hs' instead of 'TargetModule A'.
        -- In 'fromTargetId', we dutifully look for '.hs-boot' files and add them to the
        -- targetLocations of the TargetDetails. Then we add everything to the 'knownTargetsVar'.
        -- However, when we look for a 'Foo.hs-boot' file in 'FindImports.hs', we look for either
        --
        --  * TargetFile Foo.hs-boot
        --  * TargetModule Foo
        --
        -- If we don't generate a TargetFile for each potential location, we will only have
        -- 'TargetFile Foo.hs' in the 'knownTargetsVar', thus not find 'TargetFile Foo.hs-boot'
        -- and also not find 'TargetModule Foo'.
        fs <- filterM (IO.doesFileExist . fromNormalizedFilePath) targetLocations
        pure $ map (\fp -> (TargetFile fp, Set.singleton fp)) (nubOrd (f:fs))
      TargetModule _ -> do
        found <- filterM (IO.doesFileExist . fromNormalizedFilePath) targetLocations
        return [(targetTarget, Set.fromList found)]
  hasUpdate <- atomically $ do
    known <- readTVar knownTargetsVar
    let known' = flip mapHashed known $ \k -> unionKnownTargets k (mkKnownTargets knownTargets)
        hasUpdate = if known /= known' then Just (unhashed known') else Nothing
    writeTVar knownTargetsVar known'
    pure hasUpdate
  for_ hasUpdate $ \x ->
    logWith recorder Debug $ LogKnownFilesUpdated (targetMap x)
  return $ toNoFileKey GetKnownTargets


loadCradleWithNotifications ::
  Recorder (WithPriority Log) ->
  SessionState ->
  (Maybe FilePath -> FilePath -> IO (Cradle Void)) ->
  Maybe FilePath ->
  FilePath ->
  SessionM (Cradle Void, Either [CradleError] (ComponentOptions, FilePath, String))
loadCradleWithNotifications recorder sessionState loadCradle hieYaml cfp = do
  IdeTesting isTesting <- asks (optTesting . sessionIdeOptions)
  sessionPref <- asks (sessionLoading . sessionClientConfig)
  lspEnv <- asks sessionLspContext
  rootDir <- asks sessionRootDir
  let lfpLog = makeRelative rootDir cfp
  logWith recorder Info $ LogCradlePath lfpLog
  when (isNothing hieYaml) $
    logWith recorder Warning $ LogCradleNotFound lfpLog
  cradle <- liftIO $ loadCradle hieYaml rootDir
  when (isTesting) $ mRunLspT lspEnv $
    sendNotification (SMethod_CustomMethod (Proxy @"ghcide/cradle/loaded")) (toJSON cfp)

  -- Display a user friendly progress message here: They probably don't know what a cradle is
  let progMsg = "Setting up " <> T.pack (takeBaseName (cradleRootDir cradle))
                <> " (for " <> T.pack lfpLog <> ")"

  extraToLoads <- liftIO $ getExtraFilesToLoad sessionState cfp
  eopts <- mRunLspTCallback lspEnv (\act -> withIndefiniteProgress progMsg Nothing NotCancellable (const act)) $
    withTrace "Load cradle" $ \addTag -> do
        addTag "file" lfpLog
        res <- liftIO $ cradleToOptsAndLibDir recorder sessionPref cradle cfp extraToLoads
        addTag "result" (show res)
        return res
  pure (cradle, eopts)


-- | Run the specific cradle on a specific FilePath via hie-bios.
-- This then builds dependencies or whatever based on the cradle, gets the
-- GHC options/dynflags needed for the session and the GHC library directory
cradleToOptsAndLibDir :: Recorder (WithPriority Log) -> SessionLoadingPreferenceConfig -> Cradle Void -> FilePath -> [FilePath]
                      -> IO (Either [CradleError] (ComponentOptions, FilePath, String))
cradleToOptsAndLibDir recorder loadConfig cradle file old_fps = do
    -- let noneCradleFoundMessage :: FilePath -> T.Text
    --     noneCradleFoundMessage f = T.pack $ "none cradle found for " <> f <> ", ignoring the file"
    -- Start off by getting the session options
    logWith recorder Debug $ LogCradle cradle
    cradleRes <- HieBios.getCompilerOptions file loadStyle cradle
    case cradleRes of
        CradleSuccess r -> do
            -- Now get the GHC lib dir
            libDirRes <- getRuntimeGhcLibDir cradle
            versionRes <- getRuntimeGhcVersion cradle
            case liftA2 (,) libDirRes versionRes of
                -- This is the successful path
                (CradleSuccess (libDir, version)) -> pure (Right (r, libDir, version))
                CradleFail err       -> return (Left [err])
                CradleNone           -> do
                    logWith recorder Info $ LogNoneCradleFound file
                    return (Left [])

        CradleFail err -> return (Left [err])
        CradleNone -> do
            logWith recorder Info $ LogNoneCradleFound file
            return (Left [])

    where
        loadStyle = case loadConfig of
            PreferSingleComponentLoading -> LoadFile
            PreferMultiComponentLoading  -> LoadWithContext old_fps

-- ----------------------------------------------------------------------------
-- Utilities
-- ----------------------------------------------------------------------------

emptyHscEnvM :: FilePath -> SessionM HscEnv
emptyHscEnvM libDir = do
  nc <- asks sessionSharedNameCache
  liftIO $ Ghc.emptyHscEnv nc libDir

toFlagsMap :: TargetDetails -> [(NormalizedFilePath, (IdeResult HscEnvEq, DependencyInfo))]
toFlagsMap TargetDetails{..} =
    [ (l, (targetEnv, targetDepends)) | l <-  targetLocations]

-- See Note [Multi Cradle Dependency Info]
type HieMap = Map.Map (Maybe FilePath) [RawComponentInfo]
-- | Maps a "hie.yaml" location to all its Target Filepaths and options.
type FlagsMap = STM.Map (Maybe FilePath) (HM.HashMap NormalizedFilePath (IdeResult HscEnvEq, DependencyInfo))
-- | Maps a Filepath to its respective "hie.yaml" location.
-- It aims to be the reverse of 'FlagsMap'.
type FilesMap = STM.Map NormalizedFilePath (Maybe FilePath)

-- | Memoize an IO function, with the characteristics:
--
--   * If multiple people ask for a result simultaneously, make sure you only compute it once.
--
--   * If there are exceptions, repeatedly reraise them.
--
--   * If the caller is aborted (async exception) finish computing it anyway.
memoIO :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoIO op = do
    ref <- newVar Map.empty
    return $ \k -> join $ mask_ $ modifyVar ref $ \mp ->
        case Map.lookup k mp of
            Nothing -> do
                res <- onceFork $ op k
                return (Map.insert k res mp, res)
            Just res -> return (mp, res)

----------------------------------------------------------------------------------------------------

data PackageSetupException
    = PackageSetupException
        { message     :: !String
        }
    | GhcVersionMismatch
        { compileTime :: !Version
        , runTime     :: !Version
        }
    deriving (Eq, Show, Typeable)

instance Exception PackageSetupException

showPackageSetupException :: PackageSetupException -> String
showPackageSetupException GhcVersionMismatch{..} = unwords
    ["ghcide compiled against GHC"
    ,showVersion compileTime
    ,"but currently using"
    ,showVersion runTime
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC version as the project."
    ]
showPackageSetupException PackageSetupException{..} = unwords
    [ "ghcide compiled by GHC", showVersion fullCompilerVersion
    , "failed to load packages:", message <> "."
    , "\nPlease ensure that ghcide is compiled with the same GHC installation as the project."]

renderPackageSetupException :: FilePath -> PackageSetupException -> FileDiagnostic
renderPackageSetupException fp e =
  ideErrorWithSource (Just "cradle") (Just DiagnosticSeverity_Error) (toNormalizedFilePath' fp) (T.pack $ showPackageSetupException e) Nothing
