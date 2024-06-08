{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

{-|
The logic for setting up a ghcide session by tapping into hie-bios.
-}
module Development.IDE.Session
  (SessionLoadingOptions(..)
  ,CacheDirs(..)
  ,loadSessionWithOptions
  ,setInitialDynFlags
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
import           Control.Exception.Safe               as Safe
import           Control.Monad
import           Control.Monad.Extra                  as Extra
import           Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                     as H
import           Data.Aeson                           hiding (Error)
import           Data.Bifunctor
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Char8                as B
import           Data.Default
import           Data.Either.Extra
import           Data.Function
import           Data.Hashable                        hiding (hash)
import qualified Data.HashMap.Strict                  as HM
import           Data.IORef
import           Data.List
import           Data.List.Extra                      as L
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map.Strict                      as Map
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                            as T
import           Data.Time.Clock
import           Data.Version
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake           hiding (Log, knownTargets,
                                                       withHieDb)
import qualified Development.IDE.GHC.Compat           as Compat
import           Development.IDE.GHC.Compat.CmdLine
import           Development.IDE.GHC.Compat.Core      hiding (Target,
                                                       TargetFile, TargetModule,
                                                       Var, Warning, getOptions)
import qualified Development.IDE.GHC.Compat.Core      as GHC
import           Development.IDE.GHC.Compat.Env       hiding (Logger)
import           Development.IDE.GHC.Compat.Units     (UnitId)
import           Development.IDE.GHC.Util
import           Development.IDE.Graph                (Action)
import qualified Development.IDE.Session.Implicit     as GhcIde
import           Development.IDE.Session.VersionCheck
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq       (HscEnvEq, newHscEnvEq,
                                                       newHscEnvEqPreserveImportPaths)
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           GHC.Check
import           GHC.ResponseFile
import qualified HIE.Bios                             as HieBios
import           HIE.Bios.Environment                 hiding (getCacheDir)
import           HIE.Bios.Types                       hiding (Log)
import qualified HIE.Bios.Types                       as HieBios
import           Ide.Logger                           (Pretty (pretty),
                                                       Priority (Debug, Error, Info, Warning),
                                                       Recorder, WithPriority,
                                                       cmapWithPrio, logWith,
                                                       nest,
                                                       toCologActionWithPrio,
                                                       vcat, viaShow, (<+>))
import           Ide.Types                            (SessionLoadingPreferenceConfig (..),
                                                       sessionLoading)
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
import           Control.Monad.Trans.Cont             (ContT (ContT, runContT))
import           Data.Foldable                        (for_)
import           Data.HashMap.Strict                  (HashMap)
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as Set
import           Database.SQLite.Simple
import           Development.IDE.Core.Tracing         (withTrace)
import           Development.IDE.Core.WorkerThread    (awaitRunInThread,
                                                       withWorkerQueue)
import           Development.IDE.Session.Diagnostics  (renderCradleError)
import           Development.IDE.Types.Shake          (WithHieDb,
                                                       WithHieDbShield (..),
                                                       toNoFileKey)
import           HieDb.Create
import           HieDb.Types
import           HieDb.Utils
import           Ide.PluginUtils                      (toAbsolute)
import qualified System.Random                        as Random
import           System.Random                        (RandomGen)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if MIN_VERSION_ghc(9,3,0)
import qualified Data.Set                             as OS
import qualified Development.IDE.GHC.Compat.Util      as Compat
import           GHC.Data.Graph.Directed

import           GHC.Data.Bag
import           GHC.Driver.Env                       (hsc_all_home_unit_ids)
import           GHC.Driver.Errors.Types
import           GHC.Types.Error                      (errMsgDiagnostic,
                                                       singleMessage)
import           GHC.Unit.State
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
  | LogSessionLoadingChanged
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
    LogSessionLoadingChanged ->
      "Session Loading config changed, reloading the full session."

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
  , loadCradle             :: Recorder (WithPriority Log) -> Maybe FilePath -> FilePath -> IO (HieBios.Cradle Void)
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
  cradle_files <- newIORef []
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
  biosSessionLoadingVar <- newVar Nothing :: IO (Var (Maybe SessionLoadingPreferenceConfig))
  let returnWithVersion fun = IdeGhcSession fun <$> liftIO (readVar version)
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
    extras@ShakeExtras{restartShakeSession, ideNc, knownTargetsVar, lspEnv
                      } <- getShakeExtras
    let invalidateShakeCache = do
            void $ modifyVar' version succ
            return $ toNoFileKey GhcSessionIO

    IdeOptions{ optTesting = IdeTesting optTesting
              , optCheckProject = getCheckProject
              , optExtensions
              } <- getIdeOptions

        -- populate the knownTargetsVar with all the
        -- files in the project so that `knownFiles` can learn about them and
        -- we can generate a complete module graph
    let extendKnownTargets newTargets = do
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
            let known' = flip mapHashed known $ \k ->
                            HM.unionWith (<>) k $ HM.fromList knownTargets
                hasUpdate = if known /= known' then Just (unhashed known') else Nothing
            writeTVar knownTargetsVar known'
            pure hasUpdate
          for_ hasUpdate $ \x ->
            logWith recorder Debug $ LogKnownFilesUpdated x
          return $ toNoFileKey GetKnownTargets

    -- Create a new HscEnv from a hieYaml root and a set of options
    let packageSetup :: (Maybe FilePath, NormalizedFilePath, ComponentOptions, FilePath)
                     -> IO ([ComponentInfo], [ComponentInfo])
        packageSetup (hieYaml, cfp, opts, libDir) = do
          -- Parse DynFlags for the newly discovered component
          hscEnv <- emptyHscEnv ideNc libDir
          newTargetDfs <- evalGhcEnv hscEnv $ setOptions cfp opts (hsc_dflags hscEnv) rootDir
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
                  new_deps = fmap (\(df, targets) -> RawComponentInfo (homeUnitId_ df) df targets cfp opts dep_info) newTargetDfs
                  all_deps = new_deps `NE.appendList` fromMaybe [] oldDeps
                  -- Get all the unit-ids for things in this component
                  _inplace = map rawComponentUnitId $ NE.toList all_deps

              all_deps' <- forM all_deps $ \RawComponentInfo{..} -> do
                  -- Remove all inplace dependencies from package flags for
                  -- components in this HscEnv
#if MIN_VERSION_ghc(9,3,0)
                  let (df2, uids) = (rawComponentDynFlags, [])
#else
                  let (df2, uids) = _removeInplacePackages fakeUid _inplace rawComponentDynFlags
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
                  pure $ ComponentInfo
                           { componentUnitId = rawComponentUnitId
                           , componentDynFlags = processed_df
                           , componentInternalUnits = uids
                           , componentTargets = rawComponentTargets
                           , componentFP = rawComponentFP
                           , componentCOptions = rawComponentCOptions
                           , componentDependencyInfo = rawComponentDependencyInfo
                           }
              -- Modify the map so the hieYaml now maps to the newly updated
              -- ComponentInfos
              -- Returns
              -- . The information for the new component which caused this cache miss
              -- . The modified information (without -inplace flags) for
              --   existing packages
              let (new,old) = NE.splitAt (NE.length new_deps) all_deps'
              pure (Map.insert hieYaml (NE.toList all_deps) m, (new,old))


    let session :: (Maybe FilePath, NormalizedFilePath, ComponentOptions, FilePath)
                -> IO (IdeResult HscEnvEq,[FilePath])
        session args@(hieYaml, _cfp, _opts, _libDir) = do
          (new_deps, old_deps) <- packageSetup args

          -- For each component, now make a new HscEnvEq which contains the
          -- HscEnv for the hie.yaml file but the DynFlags for that component
          -- For GHC's supporting multi component sessions, we create a shared
          -- HscEnv but set the active component accordingly
          hscEnv <- emptyHscEnv ideNc _libDir
          let new_cache = newComponentCache recorder optExtensions hieYaml _cfp hscEnv
          all_target_details <- new_cache old_deps new_deps rootDir

          this_dep_info <- getDependencyInfo $ maybeToList hieYaml
          let (all_targets, this_flags_map, this_options)
                = case HM.lookup _cfp flags_map' of
                    Just this -> (all_targets', flags_map', this)
                    Nothing -> (this_target_details : all_targets', HM.insert _cfp this_flags flags_map', this_flags)
                  where all_targets' = concat all_target_details
                        flags_map' = HM.fromList (concatMap toFlagsMap all_targets')
                        this_target_details = TargetDetails (TargetFile _cfp) this_error_env this_dep_info [_cfp]
                        this_flags = (this_error_env, this_dep_info)
                        this_error_env = ([this_error], Nothing)
                        this_error = ideErrorWithSource (Just "cradle") (Just DiagnosticSeverity_Error) _cfp
                                       $ T.unlines
                                       [ "No cradle target found. Is this file listed in the targets of your cradle?"
                                       , "If you are using a .cabal file, please ensure that this module is listed in either the exposed-modules or other-modules section"
                                       ]

          void $ modifyVar' fileToFlags $ Map.insert hieYaml this_flags_map
          void $ modifyVar' filesMap $ flip HM.union (HM.fromList (map ((,hieYaml) . fst) $ concatMap toFlagsMap all_targets))
          -- The VFS doesn't change on cradle edits, re-use the old one.
          -- Invalidate all the existing GhcSession build nodes by restarting the Shake session
          keys2 <- invalidateShakeCache
          restartShakeSession VFSUnmodified "new component" [] $ do
            keys1 <- extendKnownTargets all_targets
            return [keys1, keys2]

          -- Typecheck all files in the project on startup
          checkProject <- getCheckProject
          unless (null new_deps || not checkProject) $ do
                cfps' <- liftIO $ filterM (IO.doesFileExist . fromNormalizedFilePath) (concatMap targetLocations all_targets)
                void $ shakeEnqueue extras $ mkDelayedAction "InitialLoad" Debug $ void $ do
                    mmt <- uses GetModificationTime cfps'
                    let cs_exist = catMaybes (zipWith (<$) cfps' mmt)
                    modIfaces <- uses GetModIface cs_exist
                    -- update exports map
                    shakeExtras <- getShakeExtras
                    let !exportsMap' = createExportsMap $ mapMaybe (fmap hirModIface) modIfaces
                    liftIO $ atomically $ modifyTVar' (exportsMap shakeExtras) (exportsMap' <>)

          return $ second Map.keys this_options

    let consultCradle :: Maybe FilePath -> FilePath -> IO (IdeResult HscEnvEq, [FilePath])
        consultCradle hieYaml cfp = do
           let lfpLog = makeRelative rootDir cfp
           logWith recorder Info $ LogCradlePath lfpLog
           when (isNothing hieYaml) $
             logWith recorder Warning $ LogCradleNotFound lfpLog
           cradle <- loadCradle recorder hieYaml rootDir
           when optTesting $ mRunLspT lspEnv $
            sendNotification (SMethod_CustomMethod (Proxy @"ghcide/cradle/loaded")) (toJSON cfp)

           -- Display a user friendly progress message here: They probably don't know what a cradle is
           let progMsg = "Setting up " <> T.pack (takeBaseName (cradleRootDir cradle))
                         <> " (for " <> T.pack lfpLog <> ")"
           eopts <- mRunLspTCallback lspEnv (\act -> withIndefiniteProgress progMsg Nothing NotCancellable (const act)) $
              withTrace "Load cradle" $ \addTag -> do
                  addTag "file" lfpLog
                  old_files <- readIORef cradle_files
                  res <- cradleToOptsAndLibDir recorder (sessionLoading clientConfig) cradle cfp old_files
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
                 InstallationChecked _compileTime _ghcLibCheck -> do
                   atomicModifyIORef' cradle_files (\xs -> (cfp:xs,()))
                   session (hieYaml, toNormalizedFilePath' cfp, opts, libDir)
             -- Failure case, either a cradle error or the none cradle
             Left err -> do
               dep_info <- getDependencyInfo (maybeToList hieYaml)
               let ncfp = toNormalizedFilePath' cfp
               let res = (map (\err' -> renderCradleError err' cradle ncfp) err, Nothing)
               void $ modifyVar' fileToFlags $
                    Map.insertWith HM.union hieYaml (HM.singleton ncfp (res, dep_info))
               void $ modifyVar' filesMap $ HM.insert ncfp hieYaml
               return (res, maybe [] pure hieYaml ++ concatMap cradleErrorDependencies err)

    let
        -- | We allow users to specify a loading strategy.
        -- Check whether this config was changed since the last time we have loaded
        -- a session.
        --
        -- If the loading configuration changed, we likely should restart the session
        -- in its entirety.
        didSessionLoadingPreferenceConfigChange :: IO Bool
        didSessionLoadingPreferenceConfigChange = do
          mLoadingConfig <- readVar biosSessionLoadingVar
          case mLoadingConfig of
            Nothing -> do
              writeVar biosSessionLoadingVar (Just (sessionLoading clientConfig))
              pure False
            Just loadingConfig -> do
              writeVar biosSessionLoadingVar (Just (sessionLoading clientConfig))
              pure (loadingConfig /= sessionLoading clientConfig)

    -- This caches the mapping from hie.yaml + Mod.hs -> [String]
    -- Returns the Ghc session and the cradle dependencies
    let sessionOpts :: (Maybe FilePath, FilePath)
                    -> IO (IdeResult HscEnvEq, [FilePath])
        sessionOpts (hieYaml, file) = do
          Extra.whenM didSessionLoadingPreferenceConfigChange $ do
            logWith recorder Info LogSessionLoadingChanged
            -- If the dependencies are out of date then clear both caches and start
            -- again.
            modifyVar_ fileToFlags (const (return Map.empty))
            modifyVar_ filesMap (const (return HM.empty))
            -- Don't even keep the name cache, we start from scratch here!
            modifyVar_ hscEnvs (const (return Map.empty))

          v <- Map.findWithDefault HM.empty hieYaml <$> readVar fileToFlags
          let cfp = toAbsolutePath file
          case HM.lookup (toNormalizedFilePath' cfp) v of
            Just (opts, old_di) -> do
              deps_ok <- checkDependencyInfo old_di
              if not deps_ok
                then do
                  -- If the dependencies are out of date then clear both caches and start
                  -- again.
                  modifyVar_ fileToFlags (const (return Map.empty))
                  modifyVar_ filesMap (const (return HM.empty))
                  -- Keep the same name cache
                  modifyVar_ hscEnvs (return . Map.adjust (const []) hieYaml )
                  consultCradle hieYaml cfp
                else return (opts, Map.keys old_di)
            Nothing -> consultCradle hieYaml cfp

    -- The main function which gets options for a file. We only want one of these running
    -- at a time. Therefore the IORef contains the currently running cradle, if we try
    -- to get some more options then we wait for the currently running action to finish
    -- before attempting to do so.
    let getOptions :: FilePath -> IO (IdeResult HscEnvEq, [FilePath])
        getOptions file = do
            let ncfp = toNormalizedFilePath' (toAbsolutePath file)
            cachedHieYamlLocation <- HM.lookup ncfp <$> readVar filesMap
            hieYaml <- cradleLoc file
            sessionOpts (join cachedHieYamlLocation <|> hieYaml, file) `Safe.catch` \e ->
                return (([renderPackageSetupException file e], Nothing), maybe [] pure hieYaml)

    returnWithVersion $ \file -> do
      -- see Note [Serializing runs in separate thread]
      awaitRunInThread que $ getOptions file

-- | Run the specific cradle on a specific FilePath via hie-bios.
-- This then builds dependencies or whatever based on the cradle, gets the
-- GHC options/dynflags needed for the session and the GHC library directory
cradleToOptsAndLibDir :: Recorder (WithPriority Log) -> SessionLoadingPreferenceConfig -> Cradle Void -> FilePath -> [FilePath]
                      -> IO (Either [CradleError] (ComponentOptions, FilePath))
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

    where
        loadStyle = case loadConfig of
            PreferSingleComponentLoading -> LoadFile
            PreferMultiComponentLoading  -> LoadWithContext old_fps

#if MIN_VERSION_ghc(9,3,0)
emptyHscEnv :: NameCache -> FilePath -> IO HscEnv
#else
emptyHscEnv :: IORef NameCache -> FilePath -> IO HscEnv
#endif
emptyHscEnv nc libDir = do
    -- We call setSessionDynFlags so that the loader is initialised
    -- We need to do this before we call initUnits.
    env <- runGhc (Just libDir) $
      getSessionDynFlags >>= setSessionDynFlags >> getSession
    -- On GHC 9.2 calling setSessionDynFlags caches the unit databases
    -- for an empty environment. This prevents us from reading the
    -- package database subsequently. So clear the unit db cache in
    -- hsc_unit_dbs
    pure $ setNameCache nc (hscSetFlags ((hsc_dflags env){useUnicode = True }) env)
#if !MIN_VERSION_ghc(9,3,0)
              {hsc_unit_dbs = Nothing}
#endif

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
    let locs = fmap toNormalizedFilePath' fps
    return [TargetDetails (TargetModule modName) env dep locs]
-- For a 'TargetFile' we consider all the possible module names
fromTargetId _ _ (GHC.TargetFile f _) env deps = do
    let nf = toNormalizedFilePath' f
    let other
          | "-boot" `isSuffixOf` f = toNormalizedFilePath' (L.dropEnd 5 $ fromNormalizedFilePath nf)
          | otherwise = toNormalizedFilePath' (fromNormalizedFilePath nf ++ "-boot")
    return [TargetDetails (TargetFile nf) env deps [nf, other]]

toFlagsMap :: TargetDetails -> [(NormalizedFilePath, (IdeResult HscEnvEq, DependencyInfo))]
toFlagsMap TargetDetails{..} =
    [ (l, (targetEnv, targetDepends)) | l <-  targetLocations]


#if MIN_VERSION_ghc(9,3,0)
setNameCache :: NameCache -> HscEnv -> HscEnv
#else
setNameCache :: IORef NameCache -> HscEnv -> HscEnv
#endif
setNameCache nc hsc = hsc { hsc_NC = nc }

#if MIN_VERSION_ghc(9,3,0)
-- This function checks the important property that if both p and q are home units
-- then any dependency of p, which transitively depends on q is also a home unit.
-- GHC had an implementation of this function, but it was horribly inefficient
-- We should move back to the GHC implementation on compilers where
-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12162 is included
checkHomeUnitsClosed' ::  UnitEnv -> OS.Set UnitId -> [DriverMessages]
checkHomeUnitsClosed' ue home_id_set
    | OS.null bad_unit_ids = []
    | otherwise = [singleMessage $ GHC.mkPlainErrorMsgEnvelope rootLoc $ DriverHomePackagesNotClosed (OS.toList bad_unit_ids)]
  where
    bad_unit_ids = upwards_closure OS.\\ home_id_set
    rootLoc = mkGeneralSrcSpan (Compat.fsLit "<command line>")

    graph :: Graph (Node UnitId UnitId)
    graph = graphFromEdgedVerticesUniq graphNodes

    -- downwards closure of graph
    downwards_closure
      = graphFromEdgedVerticesUniq [ DigraphNode uid uid (OS.toList deps)
                                   | (uid, deps) <- Map.toList (allReachable graph node_key)]

    inverse_closure = transposeG downwards_closure

    upwards_closure = OS.fromList $ map node_key $ reachablesG inverse_closure [DigraphNode uid uid [] | uid <- OS.toList home_id_set]

    all_unit_direct_deps :: UniqMap UnitId (OS.Set UnitId)
    all_unit_direct_deps
      = unitEnv_foldWithKey go emptyUniqMap $ ue_home_unit_graph ue
      where
        go rest this this_uis =
           plusUniqMap_C OS.union
             (addToUniqMap_C OS.union external_depends this (OS.fromList $ this_deps))
             rest
           where
             external_depends = mapUniqMap (OS.fromList . unitDepends)
#if !MIN_VERSION_ghc(9,7,0)
                              $ listToUniqMap $ Map.toList
#endif

                              $ unitInfoMap this_units
             this_units = homeUnitEnv_units this_uis
             this_deps = [ Compat.toUnitId unit | (unit,Just _) <- explicitUnits this_units]

    graphNodes :: [Node UnitId UnitId]
    graphNodes = go OS.empty home_id_set
      where
        go done todo
          = case OS.minView todo of
              Nothing -> []
              Just (uid, todo')
                | OS.member uid done -> go done todo'
                | otherwise -> case lookupUniqMap all_unit_direct_deps uid of
                    Nothing -> pprPanic "uid not found" (Compat.ppr (uid, all_unit_direct_deps))
                    Just depends ->
                      let todo'' = (depends OS.\\ done) `OS.union` todo'
                      in DigraphNode uid uid (OS.toList depends) : go (OS.insert uid done) todo''
#endif

-- | Create a mapping from FilePaths to HscEnvEqs
-- This combines all the components we know about into
-- an appropriate session, which is a multi component
-- session on GHC 9.4+
newComponentCache
         :: Recorder (WithPriority Log)
         -> [String]           -- ^ File extensions to consider
         -> Maybe FilePath     -- ^ Path to cradle
         -> NormalizedFilePath -- ^ Path to file that caused the creation of this component
         -> HscEnv             -- ^ An empty HscEnv
         -> [ComponentInfo]    -- ^ New components to be loaded
         -> [ComponentInfo]    -- ^ old, already existing components
         -> FilePath           -- ^ root dir, see Note [Root Directory]
         -> IO [ [TargetDetails] ]
newComponentCache recorder exts cradlePath _cfp hsc_env old_cis new_cis dir = do
    let cis = Map.unionWith unionCIs (mkMap new_cis) (mkMap old_cis)
        -- When we have multiple components with the same uid,
        -- prefer the new one over the old.
        -- However, we might have added some targets to the old unit
        -- (see special target), so preserve those
        unionCIs new_ci old_ci = new_ci { componentTargets = componentTargets new_ci ++ componentTargets old_ci }
        mkMap = Map.fromListWith unionCIs . map (\ci -> (componentUnitId ci, ci))
    let dfs = map componentDynFlags $ Map.elems cis
        uids = Map.keys cis
    logWith recorder Info $ LogMakingNewHscEnv uids
    hscEnv' <- -- Set up a multi component session with the other units on GHC 9.4
              Compat.initUnits dfs hsc_env

#if MIN_VERSION_ghc(9,3,0)
    let closure_errs = checkHomeUnitsClosed' (hsc_unit_env hscEnv') (hsc_all_home_unit_ids hscEnv')
        multi_errs = map (ideErrorWithSource (Just "cradle") (Just DiagnosticSeverity_Warning) _cfp . T.pack . Compat.printWithoutUniques) closure_errs
        bad_units = OS.fromList $ concat $ do
            x <- bagToList $ mapBag errMsgDiagnostic $ unionManyBags $ map Compat.getMessages closure_errs
            DriverHomePackagesNotClosed us <- pure x
            pure us
        isBad ci = (homeUnitId_ (componentDynFlags ci)) `OS.member` bad_units
#else
    let isBad = const False
        multi_errs = []
#endif
    -- Whenever we spin up a session on Linux, dynamically load libm.so.6
    -- in. We need this in case the binary is statically linked, in which
    -- case the interactive session will fail when trying to load
    -- ghc-prim, which happens whenever Template Haskell is being
    -- evaluated or haskell-language-server's eval plugin tries to run
    -- some code. If the binary is dynamically linked, then this will have
    -- no effect.
    -- See https://github.com/haskell/haskell-language-server/issues/221
    -- We need to do this after the call to setSessionDynFlags initialises
    -- the loader
    when (os == "linux") $ do
      initObjLinker hscEnv'
      res <- loadDLL hscEnv' "libm.so.6"
      case res of
        Nothing  -> pure ()
        Just err -> logWith recorder Error $ LogDLLLoadError err

    forM (Map.elems cis) $ \ci -> do
      let df = componentDynFlags ci
      let createHscEnvEq = maybe newHscEnvEqPreserveImportPaths (newHscEnvEq dir) cradlePath
      thisEnv <- do
#if MIN_VERSION_ghc(9,3,0)
            -- In GHC 9.4 we have multi component support, and we have initialised all the units
            -- above.
            -- We just need to set the current unit here
            pure $ hscSetActiveUnitId (homeUnitId_ df) hscEnv'
#else
            -- This initializes the units for GHC 9.2
            -- Add the options for the current component to the HscEnv
            -- We want to call `setSessionDynFlags` instead of `hscSetFlags`
            -- because `setSessionDynFlags` also initializes the package database,
            -- which we need for any changes to the package flags in the dynflags
            -- to be visible.
            -- See #2693
            evalGhcEnv hscEnv' $ do
              _ <- setSessionDynFlags df
              getSession
#endif
      henv <- createHscEnvEq thisEnv (zip uids dfs)
      let targetEnv = (if isBad ci then multi_errs else [], Just henv)
          targetDepends = componentDependencyInfo ci
      logWith recorder Debug $ LogNewComponentCache (targetEnv, targetDepends)
      evaluate $ liftRnf rwhnf $ componentTargets ci

      let mk t = fromTargetId (importPaths df) exts (targetId t) targetEnv targetDepends
      ctargets <- concatMapM mk (componentTargets ci)

      return (L.nubOrdOn targetTarget ctargets)

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

-- See Note [Multi Cradle Dependency Info]
type DependencyInfo = Map.Map FilePath (Maybe UTCTime)
type HieMap = Map.Map (Maybe FilePath) [RawComponentInfo]
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
  , componentInternalUnits  :: [UnitId]
  -- | All targets of this components.
  , componentTargets        :: [GHC.Target]
  -- | Filepath which caused the creation of this component
  , componentFP             :: NormalizedFilePath
  -- | Component Options used to load the component.
  , componentCOptions       :: ComponentOptions
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

unit_flags :: [Flag (CmdLineP [String])]
unit_flags = [defFlag "unit"  (SepArg addUnit)]

addUnit :: String -> EwM (CmdLineP [String]) ()
addUnit unit_str = liftEwM $ do
  units <- getCmdLineState
  putCmdLineState (unit_str : units)

-- | Throws if package flags are unsatisfiable
setOptions :: GhcMonad m
    => NormalizedFilePath
    -> ComponentOptions
    -> DynFlags
    -> FilePath -- ^ root dir, see Note [Root Directory]
    -> m (NonEmpty (DynFlags, [GHC.Target]))
setOptions cfp (ComponentOptions theOpts compRoot _) dflags rootDir = do
    ((theOpts',_errs,_warns),units) <- processCmdLineP unit_flags [] (map noLoc theOpts)
    case NE.nonEmpty units of
      Just us -> initMulti us
      Nothing -> do
        (df, targets) <- initOne (map unLoc theOpts')
        -- A special target for the file which caused this wonderful
        -- component to be created. In case the cradle doesn't list all the targets for
        -- the component, in which case things will be horribly broken anyway.
        --
        -- When we have a singleComponent that is caused to be loaded due to a
        -- file, we assume the file is part of that component. This is useful
        -- for bare GHC sessions, such as many of the ones used in the testsuite
        --
        -- We don't do this when we have multiple components, because each
        -- component better list all targets or there will be anarchy.
        -- It is difficult to know which component to add our file to in
        -- that case.
        -- Multi unit arguments are likely to come from cabal, which
        -- does list all targets.
        --
        -- If we don't end up with a target for the current file in the end, then
        -- we will report it as an error for that file
        let abs_fp = toAbsolute rootDir (fromNormalizedFilePath cfp)
        let special_target = Compat.mkSimpleTarget df abs_fp
        pure $ (df, special_target : targets) :| []
    where
      initMulti unitArgFiles =
        forM unitArgFiles $ \f -> do
          args <- liftIO $ expandResponse [f]
          initOne args
      initOne this_opts = do
        (dflags', targets') <- addCmdOpts this_opts dflags
        let dflags'' =
#if MIN_VERSION_ghc(9,3,0)
                case unitIdString (homeUnitId_ dflags') of
                     -- cabal uses main for the unit id of all executable packages
                     -- This makes multi-component sessions confused about what
                     -- options to use for that component.
                     -- Solution: hash the options and use that as part of the unit id
                     -- This works because there won't be any dependencies on the
                     -- executable unit.
                     "main" ->
                       let hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init (map B.pack $ this_opts)
                           hashed_uid = Compat.toUnitId (Compat.stringToUnit ("main-"++hash))
                       in setHomeUnitId_ hashed_uid dflags'
                     _ -> dflags'
#else
                dflags'
#endif

        let targets = makeTargetsAbsolute root targets'
            root = case workingDirectory dflags'' of
              Nothing   -> compRoot
              Just wdir -> compRoot </> wdir
        let dflags''' =
              setWorkingDirectory root $
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
              makeDynFlagsAbsolute compRoot -- makeDynFlagsAbsolute already accounts for workingDirectory
              dflags''
        -- initPackages parses the -package flags and
        -- sets up the visibility for each component.
        -- Throws if a -package flag cannot be satisfied.
        -- This only works for GHC <9.2
        -- For GHC >= 9.2, we need to modify the unit env in the hsc_dflags, which
        -- is done later in newComponentCache
        final_flags <- liftIO $ wrapPackageSetupException $ Compat.oldInitUnits dflags'''
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
