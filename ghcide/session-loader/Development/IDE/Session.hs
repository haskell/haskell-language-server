{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
The logic for setting up a ghcide session by tapping into hie-bios.
-}
module Development.IDE.Session
  (SessionLoadingOptions(..)
  ,CacheDirs(..)
  ,loadSession
  ,loadSessionWithOptions
  ,setInitialDynFlags
  ,getHieDbLoc
  ,runWithDb
  ,retryOnSqliteBusy
  ,retryOnException
  ,Log(..)
  ) where

-- Unfortunately, we cannot use loadSession with ghc-lib since hie-bios uses
-- the real GHC library and the types are incompatible. Furthermore, when
-- building with ghc-lib we need to make this Haskell agnostic, so no hie-bios!

import           Control.Concurrent.Async
import           Control.Concurrent.Strict
import           Control.Exception.Safe               as Safe
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                     as H
import           Data.Aeson                           hiding (Error)
import           Data.Bifunctor
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Char8                as B
import           Data.Char                            (isLower)
import           Data.Default
import           Data.Either.Extra
import           Data.Function
import           Data.Hashable                        hiding (hash)
import qualified Data.HashMap.Strict                  as HM
import           Data.List
import           Data.List.Extra                      (dropPrefix, split)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                            as T
import           Data.Time.Clock
import           Data.Version
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake           hiding (Log, Priority,
                                                       knownTargets, withHieDb)
import qualified Development.IDE.GHC.Compat           as Compat
import           Development.IDE.GHC.Compat.Core      hiding (Target,
                                                       TargetFile, TargetModule,
                                                       Var, Warning, getOptions)
import qualified Development.IDE.GHC.Compat.Core      as GHC
import           Development.IDE.GHC.Compat.Env       hiding (Logger)
import           Development.IDE.GHC.Compat.Units     (UnitId)
import           Development.IDE.GHC.Util
import           Development.IDE.Graph                (Action)
import           Development.IDE.Session.VersionCheck
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq       (HscEnvEq, newHscEnvEq,
                                                       newHscEnvEqPreserveImportPaths)
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           GHC.Check
import qualified HIE.Bios                             as HieBios
import qualified HIE.Bios.Cradle                      as HieBios
import           HIE.Bios.Environment                 hiding (getCacheDir)
import           HIE.Bios.Types                       hiding (Log)
import qualified HIE.Bios.Types                       as HieBios
import           Hie.Implicit.Cradle                  (loadImplicitHieCradle)
import           Ide.Logger                           (Pretty (pretty),
                                                       Priority (Debug, Error, Info, Warning),
                                                       Recorder, WithPriority,
                                                       cmapWithPrio, logWith,
                                                       nest,
                                                       toCologActionWithPrio,
                                                       vcat, viaShow, (<+>))
import           Language.LSP.Protocol.Message
import           Language.LSP.Server
import           System.Directory
import qualified System.Directory.Extra               as IO
import           System.FilePath
import           System.Info

import           Control.Applicative                  (Alternative ((<|>)))
import           Data.Void

import           Control.Concurrent.STM.Stats         (atomically, modifyTVar',
                                                       readTVar, writeTVar)
import           Control.Concurrent.STM.TQueue
import           Control.DeepSeq
import           Control.Exception                    (evaluate)
import           Control.Monad.IO.Unlift              (MonadUnliftIO)
import           Data.Foldable                        (for_)
import           Data.HashMap.Strict                  (HashMap)
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as Set
import           Database.SQLite.Simple
import           Development.IDE.Core.Tracing         (withTrace)
import           Development.IDE.Types.Shake          (WithHieDb)
import           HieDb.Create
import           HieDb.Types
import           HieDb.Utils
import qualified System.Random                        as Random
import           System.Random                        (RandomGen)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,4,0)
import           Data.IORef
#endif

data Log
  = LogSettingInitialDynFlags
  | LogGetInitialGhcLibDirDefaultCradleFail !CradleError !FilePath !(Maybe FilePath) !(Cradle Void)
  | LogGetInitialGhcLibDirDefaultCradleNone
  | LogHieDbRetry !Int !Int !Int !SomeException
  | LogHieDbRetriesExhausted !Int !Int !Int !SomeException
  | LogHieDbWriterThreadSQLiteError !SQLError
  | LogHieDbWriterThreadException !SomeException
  | LogInterfaceFilesCacheDir !FilePath
  | LogKnownFilesUpdated !(HashMap Target (HashSet NormalizedFilePath))
  | LogMakingNewHscEnv ![UnitId]
  | LogDLLLoadError !String
  | LogCradlePath !FilePath
  | LogCradleNotFound !FilePath
  | LogSessionLoadingResult !(Either [CradleError] (ComponentOptions, FilePath))
  | LogCradle !(Cradle Void)
  | LogNoneCradleFound FilePath
  | LogNewComponentCache !(([FileDiagnostic], Maybe HscEnvEq), DependencyInfo)
  | LogHieBios HieBios.Log
deriving instance Show Log

instance Pretty Log where
  pretty = \case
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
    LogInterfaceFilesCacheDir path ->
      "Interface files cache directory:" <+> pretty path
    LogKnownFilesUpdated targetToPathsMap ->
      nest 2 $
        vcat
          [ "Known files updated:"
          , viaShow $ (HM.map . Set.map) fromNormalizedFilePath targetToPathsMap
          ]
    LogMakingNewHscEnv inPlaceUnitIds ->
      "Making new HscEnv. In-place unit ids:" <+> pretty (map show inPlaceUnitIds)
    LogDLLLoadError errorString ->
      "Error dynamically loading libm.so.6:" <+> pretty errorString
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
    LogNewComponentCache componentCache ->
      "New component cache HscEnvEq:" <+> viaShow componentCache
    LogHieBios msg -> pretty msg

-- | Bump this version number when making changes to the format of the data stored in hiedb
hiedbDataVersion :: String
hiedbDataVersion = "1"

data CacheDirs = CacheDirs
  { hiCacheDir, hieCacheDir, oCacheDir :: Maybe FilePath}

data SessionLoadingOptions = SessionLoadingOptions
  { findCradle             :: FilePath -> IO (Maybe FilePath)
  -- | Load the cradle with an optional 'hie.yaml' location.
  -- If a 'hie.yaml' is given, use it to load the cradle.
  -- Otherwise, use the provided project root directory to determine the cradle type.
  , loadCradle             :: Maybe FilePath -> FilePath -> IO (HieBios.Cradle Void)
  -- | Given the project name and a set of command line flags,
  --   return the path for storing generated GHC artifacts,
  --   or 'Nothing' to respect the cradle setting
  , getCacheDirs           :: String -> [String] -> IO CacheDirs
  -- | Return the GHC lib dir to use for the 'unsafeGlobalDynFlags'
  , getInitialGhcLibDir    :: Recorder (WithPriority Log) -> FilePath -> IO (Maybe LibDir)
#if !MIN_VERSION_ghc(9,3,0)
  , fakeUid                :: UnitId
    -- ^ unit id used to tag the internal component built by ghcide
    --   To reuse external interface files the unit ids must match,
    --   thus make sure to build them with `--this-unit-id` set to the
    --   same value as the ghcide fake uid
#endif
  }

instance Default SessionLoadingOptions where
    def =  SessionLoadingOptions
        {findCradle = HieBios.findCradle
        ,loadCradle = loadWithImplicitCradle
        ,getCacheDirs = getCacheDirsDefault
        ,getInitialGhcLibDir = getInitialGhcLibDirDefault
#if !MIN_VERSION_ghc(9,3,0)
        ,fakeUid = Compat.toUnitId (Compat.stringToUnit "main")
#endif
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
loadWithImplicitCradle :: Maybe FilePath
                          -- ^ Optional 'hie.yaml' location. Will be used if given.
                          -> FilePath
                          -- ^ Root directory of the project. Required as a fallback
                          -- if no 'hie.yaml' location is given.
                          -> IO (HieBios.Cradle Void)
loadWithImplicitCradle mHieYaml rootDir = do
  case mHieYaml of
    Just yaml -> HieBios.loadCradle yaml
    Nothing   -> loadImplicitHieCradle $ addTrailingPathSeparator rootDir

getInitialGhcLibDirDefault :: Recorder (WithPriority Log) -> FilePath -> IO (Maybe LibDir)
getInitialGhcLibDirDefault recorder rootDir = do
  hieYaml <- findCradle def rootDir
  cradle <- loadCradle def hieYaml rootDir
  libDirRes <- getRuntimeGhcLibDir (toCologActionWithPrio (cmapWithPrio LogHieBios recorder)) cradle
  case libDirRes of
      CradleSuccess libdir -> pure $ Just $ LibDir libdir
      CradleFail err -> do
        logWith recorder Error $ LogGetInitialGhcLibDirDefaultCradleFail err rootDir hieYaml cradle
        pure Nothing
      CradleNone -> do
        logWith recorder Warning LogGetInitialGhcLibDirDefaultCradleNone
        pure Nothing

-- | Sets `unsafeGlobalDynFlags` on using the hie-bios cradle and returns the GHC libdir
setInitialDynFlags :: Recorder (WithPriority Log) -> FilePath -> SessionLoadingOptions -> IO (Maybe LibDir)
setInitialDynFlags recorder rootDir SessionLoadingOptions{..} = do
  libdir <- getInitialGhcLibDir recorder rootDir
  dynFlags <- mapM dynFlagsForPrinting libdir
  logWith recorder Debug LogSettingInitialDynFlags
  mapM_ setUnsafeGlobalDynFlags dynFlags
  pure libdir

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
runWithDb :: Recorder (WithPriority Log) -> FilePath -> (WithHieDb -> IndexQueue -> IO ()) -> IO ()
runWithDb recorder fp k = do
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

    chan <- newTQueueIO

    withAsync (writerThread withWriteDbRetryable chan) $ \_ -> do
      withHieDb fp (\readDb -> k (makeWithHieDbRetryable recorder rng readDb) chan)
  where
    writerThread :: WithHieDb -> IndexQueue -> IO ()
    writerThread withHieDbRetryable chan = do
      -- Clear the index of any files that might have been deleted since the last run
      _ <- withHieDbRetryable deleteMissingRealFiles
      _ <- withHieDbRetryable garbageCollectTypeNames
      forever $ do
        l <- atomically $ readTQueue chan
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
loadSession :: Recorder (WithPriority Log) -> FilePath -> IO (Action IdeGhcSession)
loadSession recorder = loadSessionWithOptions recorder def

loadSessionWithOptions :: Recorder (WithPriority Log) -> SessionLoadingOptions -> FilePath -> IO (Action IdeGhcSession)
loadSessionWithOptions recorder SessionLoadingOptions{..} dir = do
  -- Mapping from hie.yaml file to HscEnv, one per hie.yaml file
  hscEnvs <- newVar Map.empty :: IO (Var HieMap)
  -- Mapping from a Filepath to HscEnv
  fileToFlags <- newVar Map.empty :: IO (Var FlagsMap)
  -- Mapping from a Filepath to its 'hie.yaml' location.
  -- Should hold the same Filepaths as 'fileToFlags', otherwise
  -- they are inconsistent. So, everywhere you modify 'fileToFlags',
  -- you have to modify 'filesMap' as well.
  filesMap <- newVar HM.empty :: IO (Var FilesMap)
  -- Version of the mappings above
  version <- newVar 0
  let returnWithVersion fun = IdeGhcSession fun <$> liftIO (readVar version)
  -- This caches the mapping from Mod.hs -> hie.yaml
  cradleLoc <- liftIO $ memoIO $ \v -> do
      res <- findCradle v
      -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
      -- try and normalise that
      -- e.g. see https://github.com/haskell/ghcide/issues/126
      res' <- traverse makeAbsolute res
      return $ normalise <$> res'

  dummyAs <- async $ return (error "Uninitialised")
  runningCradle <- newVar dummyAs :: IO (Var (Async (IdeResult HscEnvEq,[FilePath])))

  return $ do
    extras@ShakeExtras{restartShakeSession, ideNc, knownTargetsVar, lspEnv
                      } <- getShakeExtras
    let invalidateShakeCache :: IO ()
        invalidateShakeCache = do
            void $ modifyVar' version succ
            join $ atomically $ recordDirtyKeys extras GhcSessionIO [emptyFilePath]

    IdeOptions{ optTesting = IdeTesting optTesting
              , optCheckProject = getCheckProject
              , optExtensions
              } <- getIdeOptions

        -- populate the knownTargetsVar with all the
        -- files in the project so that `knownFiles` can learn about them and
        -- we can generate a complete module graph
    let extendKnownTargets newTargets = do
          knownTargets <- forM newTargets $ \TargetDetails{..} ->
            case targetTarget of
              TargetFile f -> pure (targetTarget, [f])
              TargetModule _ -> do
                found <- filterM (IO.doesFileExist . fromNormalizedFilePath) targetLocations
                return (targetTarget, found)
          hasUpdate <- join $ atomically $ do
            known <- readTVar knownTargetsVar
            let known' = flip mapHashed known $ \k ->
                            HM.unionWith (<>) k $ HM.fromList $ map (second Set.fromList) knownTargets
                hasUpdate = if known /= known' then Just (unhashed known') else Nothing
            writeTVar knownTargetsVar known'
            logDirtyKeys <- recordDirtyKeys extras GetKnownTargets [emptyFilePath]
            return (logDirtyKeys >> pure hasUpdate)
          for_ hasUpdate $ \x ->
            logWith recorder Debug $ LogKnownFilesUpdated x

    -- Create a new HscEnv from a hieYaml root and a set of options
    -- If the hieYaml file already has an HscEnv, the new component is
    -- combined with the components in the old HscEnv into a new HscEnv
    -- which contains the union.
    let packageSetup :: (Maybe FilePath, NormalizedFilePath, ComponentOptions, FilePath)
                     -> IO (HscEnv, ComponentInfo, [ComponentInfo])
        packageSetup (hieYaml, cfp, opts, libDir) = do
          -- Parse DynFlags for the newly discovered component
          hscEnv <- emptyHscEnv ideNc libDir
          (df', targets) <- evalGhcEnv hscEnv $ setOptions opts (hsc_dflags hscEnv)
          let df =
#if MIN_VERSION_ghc(9,3,0)
                case unitIdString (homeUnitId_ df') of
                     -- cabal uses main for the unit id of all executable packages
                     -- This makes multi-component sessions confused about what
                     -- options to use for that component.
                     -- Solution: hash the options and use that as part of the unit id
                     -- This works because there won't be any dependencies on the
                     -- executable unit.
                     "main" ->
                       let hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init (map B.pack $ componentOptions opts)
                           hashed_uid = Compat.toUnitId (Compat.stringToUnit ("main-"++hash))
                       in setHomeUnitId_ hashed_uid df'
                     _ -> df'
#else
                df'
#endif

          let deps = componentDependencies opts ++ maybeToList hieYaml
          dep_info <- getDependencyInfo deps
          -- Now lookup to see whether we are combining with an existing HscEnv
          -- or making a new one. The lookup returns the HscEnv and a list of
          -- information about other components loaded into the HscEnv
          -- (unitId, DynFlag, Targets)
          modifyVar hscEnvs $ \m -> do
              -- Just deps if there's already an HscEnv
              -- Nothing is it's the first time we are making an HscEnv
              let oldDeps = Map.lookup hieYaml m
              let -- Add the raw information about this component to the list
                  -- We will modify the unitId and DynFlags used for
                  -- compilation but these are the true source of
                  -- information.

                  new_deps = RawComponentInfo (homeUnitId_ df) df targets cfp opts dep_info
                                : maybe [] snd oldDeps
                  -- Get all the unit-ids for things in this component
                  inplace = map rawComponentUnitId new_deps

              new_deps' <- forM new_deps $ \RawComponentInfo{..} -> do
                  -- Remove all inplace dependencies from package flags for
                  -- components in this HscEnv
#if MIN_VERSION_ghc(9,3,0)
                  let (df2, uids) = (rawComponentDynFlags, [])
#else
                  let (df2, uids) = _removeInplacePackages fakeUid inplace rawComponentDynFlags
#endif
                  let prefix = show rawComponentUnitId
                  -- See Note [Avoiding bad interface files]
                  let hscComponents = sort $ map show uids
                      cacheDirOpts = hscComponents ++ componentOptions opts
                  cacheDirs <- liftIO $ getCacheDirs prefix cacheDirOpts
                  processed_df <- setCacheDirs recorder cacheDirs df2
                  -- The final component information, mostly the same but the DynFlags don't
                  -- contain any packages which are also loaded
                  -- into the same component.
                  pure $ ComponentInfo rawComponentUnitId
                                       processed_df
                                       uids
                                       rawComponentTargets
                                       rawComponentFP
                                       rawComponentCOptions
                                       rawComponentDependencyInfo
              -- Make a new HscEnv, we have to recompile everything from
              -- scratch again (for now)
              -- It's important to keep the same NameCache though for reasons
              -- that I do not fully understand
              logWith recorder Info $ LogMakingNewHscEnv inplace
              hscEnvB <- emptyHscEnv ideNc libDir
              !newHscEnv <-
                -- Add the options for the current component to the HscEnv
                evalGhcEnv hscEnvB $ do
                  _ <- setSessionDynFlags
#if !MIN_VERSION_ghc(9,3,0)
                          $ setHomeUnitId_ fakeUid
#endif
                          df
                  getSession

              -- Modify the map so the hieYaml now maps to the newly created
              -- HscEnv
              -- Returns
              -- . the new HscEnv so it can be used to modify the
              --   FilePath -> HscEnv map (fileToFlags)
              -- . The information for the new component which caused this cache miss
              -- . The modified information (without -inplace flags) for
              --   existing packages
              pure (Map.insert hieYaml (newHscEnv, new_deps) m, (newHscEnv, head new_deps', tail new_deps'))


    let session :: (Maybe FilePath, NormalizedFilePath, ComponentOptions, FilePath)
                -> IO (IdeResult HscEnvEq,[FilePath])
        session args@(hieYaml, _cfp, _opts, _libDir) = do
          (hscEnv, new, old_deps) <- packageSetup args

          -- Whenever we spin up a session on Linux, dynamically load libm.so.6
          -- in. We need this in case the binary is statically linked, in which
          -- case the interactive session will fail when trying to load
          -- ghc-prim, which happens whenever Template Haskell is being
          -- evaluated or haskell-language-server's eval plugin tries to run
          -- some code. If the binary is dynamically linked, then this will have
          -- no effect.
          -- See https://github.com/haskell/haskell-language-server/issues/221
          when (os == "linux") $ do
            initObjLinker hscEnv
            res <- loadDLL hscEnv "libm.so.6"
            case res of
              Nothing  -> pure ()
              Just err -> logWith recorder Error $ LogDLLLoadError err


          -- Make a map from unit-id to DynFlags, this is used when trying to
          -- resolve imports. (especially PackageImports)
          let uids = map (\ci -> (componentUnitId ci, componentDynFlags ci)) (new : old_deps)

          -- For each component, now make a new HscEnvEq which contains the
          -- HscEnv for the hie.yaml file but the DynFlags for that component

          -- New HscEnv for the component in question, returns the new HscEnvEq and
          -- a mapping from FilePath to the newly created HscEnvEq.
          let new_cache = newComponentCache recorder optExtensions hieYaml _cfp hscEnv uids
          (cs, res) <- new_cache new
          -- Modified cache targets for everything else in the hie.yaml file
          -- which now uses the same EPS and so on
          cached_targets <- concatMapM (fmap fst . new_cache) old_deps

          let all_targets = cs ++ cached_targets

          void $ modifyVar' fileToFlags $
              Map.insert hieYaml (HM.fromList (concatMap toFlagsMap all_targets))
          void $ modifyVar' filesMap $
              flip HM.union (HM.fromList (zip (map fst $ concatMap toFlagsMap all_targets) (repeat hieYaml)))

          void $ extendKnownTargets all_targets

          -- Invalidate all the existing GhcSession build nodes by restarting the Shake session
          invalidateShakeCache

          -- The VFS doesn't change on cradle edits, re-use the old one.
          restartShakeSession VFSUnmodified "new component" []

          -- Typecheck all files in the project on startup
          checkProject <- getCheckProject
          unless (null cs || not checkProject) $ do
                cfps' <- liftIO $ filterM (IO.doesFileExist . fromNormalizedFilePath) (concatMap targetLocations cs)
                void $ shakeEnqueue extras $ mkDelayedAction "InitialLoad" Debug $ void $ do
                    mmt <- uses GetModificationTime cfps'
                    let cs_exist = catMaybes (zipWith (<$) cfps' mmt)
                    modIfaces <- uses GetModIface cs_exist
                    -- update exports map
                    shakeExtras <- getShakeExtras
                    let !exportsMap' = createExportsMap $ mapMaybe (fmap hirModIface) modIfaces
                    liftIO $ atomically $ modifyTVar' (exportsMap shakeExtras) (exportsMap' <>)

          return (second Map.keys res)

    let consultCradle :: Maybe FilePath -> FilePath -> IO (IdeResult HscEnvEq, [FilePath])
        consultCradle hieYaml cfp = do
           lfpLog <- flip makeRelative cfp <$> getCurrentDirectory
           logWith recorder Info $ LogCradlePath lfpLog

           when (isNothing hieYaml) $
             logWith recorder Warning $ LogCradleNotFound lfpLog

           cradle <- loadCradle hieYaml dir
           -- TODO: Why are we repeating the same command we have on line 646?
           lfp <- flip makeRelative cfp <$> getCurrentDirectory

           when optTesting $ mRunLspT lspEnv $
            sendNotification (SMethod_CustomMethod (Proxy @"ghcide/cradle/loaded")) (toJSON cfp)

           -- Display a user friendly progress message here: They probably don't know what a cradle is
           let progMsg = "Setting up " <> T.pack (takeBaseName (cradleRootDir cradle))
                         <> " (for " <> T.pack lfp <> ")"
           eopts <- mRunLspTCallback lspEnv (withIndefiniteProgress progMsg NotCancellable) $
              withTrace "Load cradle" $ \addTag -> do
                  addTag "file" lfp
                  res <- cradleToOptsAndLibDir recorder cradle cfp
                  addTag "result" (show res)
                  return res

           logWith recorder Debug $ LogSessionLoadingResult eopts
           case eopts of
             -- The cradle gave us some options so get to work turning them
             -- into and HscEnv.
             Right (opts, libDir) -> do
               installationCheck <- ghcVersionChecker libDir
               case installationCheck of
                 InstallationNotFound{..} ->
                     error $ "GHC installation not found in libdir: " <> libdir
                 InstallationMismatch{..} ->
                     return (([renderPackageSetupException cfp GhcVersionMismatch{..}], Nothing),[])
                 InstallationChecked _compileTime _ghcLibCheck ->
                   session (hieYaml, toNormalizedFilePath' cfp, opts, libDir)
             -- Failure case, either a cradle error or the none cradle
             Left err -> do
               dep_info <- getDependencyInfo (maybeToList hieYaml)
               let ncfp = toNormalizedFilePath' cfp
               let res = (map (renderCradleError cradle ncfp) err, Nothing)
               void $ modifyVar' fileToFlags $
                    Map.insertWith HM.union hieYaml (HM.singleton ncfp (res, dep_info))
               void $ modifyVar' filesMap $ HM.insert ncfp hieYaml
               return (res, maybe [] pure hieYaml ++ concatMap cradleErrorDependencies err)

    -- This caches the mapping from hie.yaml + Mod.hs -> [String]
    -- Returns the Ghc session and the cradle dependencies
    let sessionOpts :: (Maybe FilePath, FilePath)
                    -> IO (IdeResult HscEnvEq, [FilePath])
        sessionOpts (hieYaml, file) = do
          v <- Map.findWithDefault HM.empty hieYaml <$> readVar fileToFlags
          cfp <- makeAbsolute file
          case HM.lookup (toNormalizedFilePath' cfp) v of
            Just (opts, old_di) -> do
              deps_ok <- checkDependencyInfo old_di
              if not deps_ok
                then do
                  -- If the dependencies are out of date then clear both caches and start
                  -- again.
                  modifyVar_ fileToFlags (const (return Map.empty))
                  -- Keep the same name cache
                  modifyVar_ hscEnvs (return . Map.adjust (\(h, _) -> (h, [])) hieYaml )
                  consultCradle hieYaml cfp
                else return (opts, Map.keys old_di)
            Nothing -> consultCradle hieYaml cfp

    -- The main function which gets options for a file. We only want one of these running
    -- at a time. Therefore the IORef contains the currently running cradle, if we try
    -- to get some more options then we wait for the currently running action to finish
    -- before attempting to do so.
    let getOptions :: FilePath -> IO (IdeResult HscEnvEq, [FilePath])
        getOptions file = do
            ncfp <- toNormalizedFilePath' <$> makeAbsolute file
            cachedHieYamlLocation <- HM.lookup ncfp <$> readVar filesMap
            hieYaml <- cradleLoc file
            sessionOpts (join cachedHieYamlLocation <|> hieYaml, file) `Safe.catch` \e ->
                return (([renderPackageSetupException file e], Nothing), maybe [] pure hieYaml)

    returnWithVersion $ \file -> do
      opts <- liftIO $ join $ mask_ $ modifyVar runningCradle $ \as -> do
        -- If the cradle is not finished, then wait for it to finish.
        void $ wait as
        asyncRes <- async $ getOptions file
        return (asyncRes, wait asyncRes)
      pure opts

-- | Run the specific cradle on a specific FilePath via hie-bios.
-- This then builds dependencies or whatever based on the cradle, gets the
-- GHC options/dynflags needed for the session and the GHC library directory
cradleToOptsAndLibDir :: Recorder (WithPriority Log) -> Cradle Void -> FilePath
                      -> IO (Either [CradleError] (ComponentOptions, FilePath))
cradleToOptsAndLibDir recorder cradle file = do
    -- let noneCradleFoundMessage :: FilePath -> T.Text
    --     noneCradleFoundMessage f = T.pack $ "none cradle found for " <> f <> ", ignoring the file"
    -- Start off by getting the session options
    logWith recorder Debug $ LogCradle cradle
    let logger = toCologActionWithPrio $ cmapWithPrio LogHieBios recorder
    cradleRes <- HieBios.getCompilerOptions logger file cradle
    case cradleRes of
        CradleSuccess r -> do
            -- Now get the GHC lib dir
            libDirRes <- getRuntimeGhcLibDir logger cradle
            case libDirRes of
                -- This is the successful path
                CradleSuccess libDir -> pure (Right (r, libDir))
                CradleFail err       -> return (Left [err])
                CradleNone           -> do
                    logWith recorder Info $ LogNoneCradleFound file
                    return (Left [])

        CradleFail err -> return (Left [err])
        CradleNone -> do
            logWith recorder Info $ LogNoneCradleFound file
            return (Left [])

#if MIN_VERSION_ghc(9,3,0)
emptyHscEnv :: NameCache -> FilePath -> IO HscEnv
#else
emptyHscEnv :: IORef NameCache -> FilePath -> IO HscEnv
#endif
emptyHscEnv nc libDir = do
    env <- runGhc (Just libDir) getSession
    pure $ setNameCache nc (hscSetFlags ((hsc_dflags env){useUnicode = True }) env)

data TargetDetails = TargetDetails
  {
      targetTarget    :: !Target,
      targetEnv       :: !(IdeResult HscEnvEq),
      targetDepends   :: !DependencyInfo,
      targetLocations :: ![NormalizedFilePath]
  }

fromTargetId :: [FilePath]          -- ^ import paths
             -> [String]            -- ^ extensions to consider
             -> TargetId
             -> IdeResult HscEnvEq
             -> DependencyInfo
             -> IO [TargetDetails]
-- For a target module we consider all the import paths
fromTargetId is exts (GHC.TargetModule modName) env dep = do
    let fps = [i </> moduleNameSlashes modName -<.> ext <> boot
              | ext <- exts
              , i <- is
              , boot <- ["", "-boot"]
              ]
    locs <- mapM (fmap toNormalizedFilePath' . makeAbsolute) fps
    return [TargetDetails (TargetModule modName) env dep locs]
-- For a 'TargetFile' we consider all the possible module names
fromTargetId _ _ (GHC.TargetFile f _) env deps = do
    nf <- toNormalizedFilePath' <$> makeAbsolute f
    return [TargetDetails (TargetFile nf) env deps [nf]]

toFlagsMap :: TargetDetails -> [(NormalizedFilePath, (IdeResult HscEnvEq, DependencyInfo))]
toFlagsMap TargetDetails{..} =
    [ (l, (targetEnv, targetDepends)) | l <-  targetLocations]


#if MIN_VERSION_ghc(9,3,0)
setNameCache :: NameCache -> HscEnv -> HscEnv
#else
setNameCache :: IORef NameCache -> HscEnv -> HscEnv
#endif
setNameCache nc hsc = hsc { hsc_NC = nc }

-- | Create a mapping from FilePaths to HscEnvEqs
newComponentCache
         :: Recorder (WithPriority Log)
         -> [String]       -- File extensions to consider
         -> Maybe FilePath -- Path to cradle
         -> NormalizedFilePath -- Path to file that caused the creation of this component
         -> HscEnv
         -> [(UnitId, DynFlags)]
         -> ComponentInfo
         -> IO ( [TargetDetails], (IdeResult HscEnvEq, DependencyInfo))
newComponentCache recorder exts cradlePath cfp hsc_env uids ci = do
    let df = componentDynFlags ci
    hscEnv' <-
#if MIN_VERSION_ghc(9,3,0)
      -- Set up a multi component session with the other units on GHC 9.4
        Compat.initUnits (map snd uids) (hscSetFlags df hsc_env)
#elif MIN_VERSION_ghc(9,2,0)
      -- This initializes the units for GHC 9.2
      -- Add the options for the current component to the HscEnv
      -- We want to call `setSessionDynFlags` instead of `hscSetFlags`
      -- because `setSessionDynFlags` also initializes the package database,
      -- which we need for any changes to the package flags in the dynflags
      -- to be visible.
      -- See #2693
      evalGhcEnv hsc_env $ do
        _ <- setSessionDynFlags $ df
        getSession
#else
      -- getOptions is enough to initialize units on GHC <9.2
      pure $ hscSetFlags df hsc_env { hsc_IC = (hsc_IC hsc_env) { ic_dflags = df } }
#endif

    let newFunc = maybe newHscEnvEqPreserveImportPaths newHscEnvEq cradlePath
    henv <- newFunc hscEnv' uids
    let targetEnv = ([], Just henv)
        targetDepends = componentDependencyInfo ci
        res = (targetEnv, targetDepends)
    logWith recorder Debug $ LogNewComponentCache res
    evaluate $ liftRnf rwhnf $ componentTargets ci

    let mk t = fromTargetId (importPaths df) exts (targetId t) targetEnv targetDepends
    ctargets <- concatMapM mk (componentTargets ci)

    -- A special target for the file which caused this wonderful
    -- component to be created. In case the cradle doesn't list all the targets for
    -- the component, in which case things will be horribly broken anyway.
    -- Otherwise, we will immediately attempt to reload this module which
    -- causes an infinite loop and high CPU usage.
    let special_target = TargetDetails (TargetFile cfp) targetEnv targetDepends [componentFP ci]
    return (special_target:ctargets, res)

{- Note [Avoiding bad interface files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Originally, we set the cache directory for the various components once
on the first occurrence of the component.
This works fine if these components have no references to each other,
but you have components that depend on each other, the interface files are
updated for each component.
After restarting the session and only opening the component that depended
on the other, suddenly the interface files of this component are stale.
However, from the point of view of `ghcide`, they do not look stale,
thus, not regenerated and the IDE shows weird errors such as:
```
typecheckIface
Declaration for Rep_ClientRunFlags
Axiom branches Rep_ClientRunFlags:
  Failed to load interface for ‘Distribution.Simple.Flag’
  Use -v to see a list of the files searched for.
```
and
```
expectJust checkFamInstConsistency
CallStack (from HasCallStack):
  error, called at compiler\\utils\\Maybes.hs:55:27 in ghc:Maybes
  expectJust, called at compiler\\typecheck\\FamInst.hs:461:30 in ghc:FamInst
```

To mitigate this, we set the cache directory for each component dependent
on the components of the current `HscEnv`, additionally to the component options
of the respective components.
Assume two components, c1, c2, where c2 depends on c1, and the options of the
respective components are co1, co2.
If we want to load component c2, followed by c1, we set the cache directory for
each component in this way:

  * Load component c2
    * (Cache Directory State)
        - name of c2 + co2
  * Load component c1
    * (Cache Directory State)
        - name of c2 + name of c1 + co2
        - name of c2 + name of c1 + co1

Overall, we created three cache directories. If we opened c1 first, then we
create a fourth cache directory.
This makes sure that interface files are always correctly updated.

Since this causes a lot of recompilation, we only update the cache-directory,
if the dependencies of a component have really changed.
E.g. when you load two executables, they can not depend on each other. They
should be filtered out, such that we dont have to re-compile everything.
-}

-- | Set the cache-directory based on the ComponentOptions and a list of
-- internal packages.
-- For the exact reason, see Note [Avoiding bad interface files].
setCacheDirs :: MonadUnliftIO m => Recorder (WithPriority Log) -> CacheDirs -> DynFlags -> m DynFlags
setCacheDirs recorder CacheDirs{..} dflags = do
    logWith recorder Info $ LogInterfaceFilesCacheDir (fromMaybe cacheDir hiCacheDir)
    pure $ dflags
          & maybe id setHiDir hiCacheDir
          & maybe id setHieDir hieCacheDir
          & maybe id setODir oCacheDir


renderCradleError :: Cradle a -> NormalizedFilePath -> CradleError -> FileDiagnostic
renderCradleError cradle nfp (CradleError _ _ec ms) =
  ideErrorWithSource (Just "cradle") (Just DiagnosticSeverity_Error) nfp $ T.unlines $ map T.pack userFriendlyMessage
  where

    userFriendlyMessage :: [String]
    userFriendlyMessage
      | HieBios.isCabalCradle cradle = fromMaybe ms fileMissingMessage
      | otherwise = ms

    fileMissingMessage :: Maybe [String]
    fileMissingMessage =
      multiCradleErrMessage <$> parseMultiCradleErr ms

-- | Information included in Multi Cradle error messages
data MultiCradleErr = MultiCradleErr
  { mcPwd      :: FilePath
  , mcFilePath :: FilePath
  , mcPrefixes :: [(FilePath, String)]
  } deriving (Show)

-- | Attempt to parse a multi-cradle message
parseMultiCradleErr :: [String] -> Maybe MultiCradleErr
parseMultiCradleErr ms = do
  _  <- lineAfter "Multi Cradle: "
  wd <- lineAfter "pwd: "
  fp <- lineAfter "filepath: "
  ps <- prefixes
  pure $ MultiCradleErr wd fp ps

  where
    lineAfter :: String -> Maybe String
    lineAfter pre = listToMaybe $ mapMaybe (stripPrefix pre) ms

    prefixes :: Maybe [(FilePath, String)]
    prefixes = do
      pure $ mapMaybe tuple ms

    tuple :: String -> Maybe (String, String)
    tuple line = do
      line' <- surround '(' line ')'
      [f, s] <- pure $ split (==',') line'
      pure (f, s)

    -- extracts the string surrounded by required characters
    surround :: Char -> String -> Char -> Maybe String
    surround start s end = do
      guard (listToMaybe s == Just start)
      guard (listToMaybe (reverse s) == Just end)
      pure $ drop 1 $ take (length s - 1) s

multiCradleErrMessage :: MultiCradleErr -> [String]
multiCradleErrMessage e =
    [ "Loading the module '" <> moduleFileName <> "' failed. It may not be listed in your .cabal file!"
    , "Perhaps you need to add `"<> moduleName <> "` to other-modules or exposed-modules."
    , "For more information, visit: https://cabal.readthedocs.io/en/3.4/developing-packages.html#modules-included-in-the-package"
    , ""
    ] <> map prefix (mcPrefixes e)
  where
    localFilePath f = dropWhile (==pathSeparator) $ dropPrefix (mcPwd e) f
    moduleFileName = localFilePath $ mcFilePath e
    moduleName = intercalate "." $ map dropExtension $ dropWhile isSourceFolder $ splitDirectories moduleFileName
    isSourceFolder p = all isLower $ take 1 p
    prefix (f, r) = f <> " - " <> r

-- See Note [Multi Cradle Dependency Info]
type DependencyInfo = Map.Map FilePath (Maybe UTCTime)
type HieMap = Map.Map (Maybe FilePath) (HscEnv, [RawComponentInfo])
-- | Maps a "hie.yaml" location to all its Target Filepaths and options.
type FlagsMap = Map.Map (Maybe FilePath) (HM.HashMap NormalizedFilePath (IdeResult HscEnvEq, DependencyInfo))
-- | Maps a Filepath to its respective "hie.yaml" location.
-- It aims to be the reverse of 'FlagsMap'.
type FilesMap = HM.HashMap NormalizedFilePath (Maybe FilePath)

-- This is pristine information about a component
data RawComponentInfo = RawComponentInfo
  { rawComponentUnitId         :: UnitId
  -- | Unprocessed DynFlags. Contains inplace packages such as libraries.
  -- We do not want to use them unprocessed.
  , rawComponentDynFlags       :: DynFlags
  -- | All targets of this components.
  , rawComponentTargets        :: [GHC.Target]
  -- | Filepath which caused the creation of this component
  , rawComponentFP             :: NormalizedFilePath
  -- | Component Options used to load the component.
  , rawComponentCOptions       :: ComponentOptions
  -- | Maps cradle dependencies, such as `stack.yaml`, or `.cabal` file
  -- to last modification time. See Note [Multi Cradle Dependency Info].
  , rawComponentDependencyInfo :: DependencyInfo
  }

-- This is processed information about the component, in particular the dynflags will be modified.
data ComponentInfo = ComponentInfo
  { componentUnitId         :: UnitId
  -- | Processed DynFlags. Does not contain inplace packages such as local
  -- libraries. Can be used to actually load this Component.
  , componentDynFlags       :: DynFlags
  -- | Internal units, such as local libraries, that this component
  -- is loaded with. These have been extracted from the original
  -- ComponentOptions.
  , _componentInternalUnits :: [UnitId]
  -- | All targets of this components.
  , componentTargets        :: [GHC.Target]
  -- | Filepath which caused the creation of this component
  , componentFP             :: NormalizedFilePath
  -- | Component Options used to load the component.
  , _componentCOptions      :: ComponentOptions
  -- | Maps cradle dependencies, such as `stack.yaml`, or `.cabal` file
  -- to last modification time. See Note [Multi Cradle Dependency Info]
  , componentDependencyInfo :: DependencyInfo
  }

-- | Check if any dependency has been modified lately.
checkDependencyInfo :: DependencyInfo -> IO Bool
checkDependencyInfo old_di = do
  di <- getDependencyInfo (Map.keys old_di)
  return (di == old_di)

-- Note [Multi Cradle Dependency Info]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Why do we implement our own file modification tracking here?
-- The primary reason is that the custom caching logic is quite complicated and going into shake
-- adds even more complexity and more indirection. I did try for about 5 hours to work out how to
-- use shake rules rather than IO but eventually gave up.

-- | Computes a mapping from a filepath to its latest modification date.
-- See Note [Multi Cradle Dependency Info] why we do this ourselves instead
-- of letting shake take care of it.
getDependencyInfo :: [FilePath] -> IO DependencyInfo
getDependencyInfo fs = Map.fromList <$> mapM do_one fs

  where
    safeTryIO :: IO a -> IO (Either IOException a)
    safeTryIO = Safe.try

    do_one :: FilePath -> IO (FilePath, Maybe UTCTime)
    do_one fp = (fp,) . eitherToMaybe <$> safeTryIO (getModificationTime fp)

-- | This function removes all the -package flags which refer to packages we
-- are going to deal with ourselves. For example, if a executable depends
-- on a library component, then this function will remove the library flag
-- from the package flags for the executable
--
-- There are several places in GHC (for example the call to hptInstances in
-- tcRnImports) which assume that all modules in the HPT have the same unit
-- ID. Therefore we create a fake one and give them all the same unit id.
_removeInplacePackages --Only used in ghc < 9.4
    :: UnitId     -- ^ fake uid to use for our internal component
    -> [UnitId]
    -> DynFlags
    -> (DynFlags, [UnitId])
_removeInplacePackages fake_uid us df = (setHomeUnitId_ fake_uid $
                                       df { packageFlags = ps }, uids)
  where
    (uids, ps) = Compat.filterInplaceUnits us (packageFlags df)

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

-- | Throws if package flags are unsatisfiable
setOptions :: GhcMonad m => ComponentOptions -> DynFlags -> m (DynFlags, [GHC.Target])
setOptions (ComponentOptions theOpts compRoot _) dflags = do
    (dflags', targets') <- addCmdOpts theOpts dflags
    let targets = makeTargetsAbsolute compRoot targets'
    let dflags'' =
          disableWarningsAsErrors $
          -- disabled, generated directly by ghcide instead
          flip gopt_unset Opt_WriteInterface $
          -- disabled, generated directly by ghcide instead
          -- also, it can confuse the interface stale check
          dontWriteHieFiles $
          setIgnoreInterfacePragmas $
          setBytecodeLinkerOptions $
          disableOptimisation $
          Compat.setUpTypedHoles $
          makeDynFlagsAbsolute compRoot dflags'
    -- initPackages parses the -package flags and
    -- sets up the visibility for each component.
    -- Throws if a -package flag cannot be satisfied.
    -- This only works for GHC <9.2
    -- For GHC >= 9.2, we need to modify the unit env in the hsc_dflags, which
    -- is done later in newComponentCache
    final_flags <- liftIO $ wrapPackageSetupException $ Compat.oldInitUnits dflags''
    return (final_flags, targets)

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
    gopt_set (gopt_set df Opt_IgnoreInterfacePragmas) Opt_IgnoreOptimChanges

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { hiDir      = Just f}

setODir :: FilePath -> DynFlags -> DynFlags
setODir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { objectDir = Just f}

getCacheDirsDefault :: String -> [String] -> IO CacheDirs
getCacheDirsDefault prefix opts = do
    dir <- Just <$> getXdgDirectory XdgCache (cacheDir </> prefix ++ "-" ++ opts_hash)
    return $ CacheDirs dir dir dir
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init (map B.pack opts)

-- | Sub directory for the cache path
cacheDir :: String
cacheDir = "ghcide"

----------------------------------------------------------------------------------------------------

data PackageSetupException
    = PackageSetupException
        { message     :: !String
        }
    | GhcVersionMismatch
        { compileTime :: !Version
        , runTime     :: !Version
        }
    | PackageCheckFailed !NotCompatibleReason
    deriving (Eq, Show, Typeable)

instance Exception PackageSetupException

-- | Wrap any exception as a 'PackageSetupException'
wrapPackageSetupException :: IO a -> IO a
wrapPackageSetupException = handleAny $ \case
  e | Just (pkgE :: PackageSetupException) <- fromException e -> throwIO pkgE
  e -> (throwIO . PackageSetupException . show) e

showPackageSetupException :: PackageSetupException -> String
showPackageSetupException GhcVersionMismatch{..} = unwords
    ["ghcide compiled against GHC"
    ,showVersion compileTime
    ,"but currently using"
    ,showVersion runTime
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC version as the project."
    ]
showPackageSetupException PackageSetupException{..} = unwords
    [ "ghcide compiled by GHC", showVersion compilerVersion
    , "failed to load packages:", message <> "."
    , "\nPlease ensure that ghcide is compiled with the same GHC installation as the project."]
showPackageSetupException (PackageCheckFailed PackageVersionMismatch{..}) = unwords
    ["ghcide compiled with package "
    , packageName <> "-" <> showVersion compileTime
    ,"but project uses package"
    , packageName <> "-" <> showVersion runTime
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC installation as the project."
    ]
showPackageSetupException (PackageCheckFailed BasePackageAbiMismatch{..}) = unwords
    ["ghcide compiled with base-" <> showVersion compileTime <> "-" <> compileTimeAbi
    ,"but project uses base-" <> showVersion compileTime <> "-" <> runTimeAbi
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC installation as the project."
    ]

renderPackageSetupException :: FilePath -> PackageSetupException -> (NormalizedFilePath, ShowDiagnostic, Diagnostic)
renderPackageSetupException fp e =
    ideErrorWithSource (Just "cradle") (Just DiagnosticSeverity_Error) (toNormalizedFilePath' fp) (T.pack $ showPackageSetupException e)
