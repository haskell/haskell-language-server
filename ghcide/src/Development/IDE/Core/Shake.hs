-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE TypeFamilies              #-}

-- | A Shake implementation of the compiler service.
--
--   There are two primary locations where data lives, and both of
--   these contain much the same data:
--
-- * The Shake database (inside 'shakeDb') stores a map of shake keys
--   to shake values. In our case, these are all of type 'Q' to 'A'.
--   During a single run all the values in the Shake database are consistent
--   so are used in conjunction with each other, e.g. in 'uses'.
--
-- * The 'Values' type stores a map of keys to values. These values are
--   always stored as real Haskell values, whereas Shake serialises all 'A' values
--   between runs. To deserialise a Shake value, we just consult Values.
module Development.IDE.Core.Shake(
    IdeState, shakeSessionInit, shakeExtras,
    ShakeExtras(..), getShakeExtras, getShakeExtrasRules,
    KnownTargets, Target(..), toKnownFiles,
    IdeRule, IdeResult,
    GetModificationTime(GetModificationTime, GetModificationTime_, missingFileDiagnostics),
    shakeOpen, shakeShut,
    shakeEnqueue,
    shakeProfile,
    newSession,
    use, useNoFile, uses, useWithStaleFast, useWithStaleFast', delayedAction,
    FastResult(..),
    use_, useNoFile_, uses_,
    useWithStale, usesWithStale,
    useWithStale_, usesWithStale_,
    BadDependency(..),
    RuleBody(..),
    define, defineNoDiagnostics,
    defineEarlyCutoff,
    defineOnDisk, needOnDisk, needOnDisks,
    defineNoFile, defineEarlyCutOffNoFile,
    getDiagnostics,
    mRunLspT, mRunLspTCallback,
    getHiddenDiagnostics,
    IsIdeGlobal, addIdeGlobal, addIdeGlobalExtras, getIdeGlobalState, getIdeGlobalAction,
    getIdeGlobalExtras,
    getIdeOptions,
    getIdeOptionsIO,
    GlobalIdeOptions(..),
    HLS.getClientConfig,
    getPluginConfig,
    garbageCollect,
    knownTargets,
    setPriority,
    ideLogger,
    actionLogger,
    FileVersion(..),
    Priority(..),
    updatePositionMapping,
    deleteValue, recordDirtyKeys,
    OnDiskRule(..),
    WithProgressFunc, WithIndefiniteProgressFunc,
    ProgressEvent(..),
    DelayedAction, mkDelayedAction,
    IdeAction(..), runIdeAction,
    mkUpdater,
    -- Exposed for testing.
    Q(..),
    IndexQueue,
    HieDb,
    HieDbWriter(..),
    VFSHandle(..),
    addPersistentRule
    ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.Strict
import           Control.DeepSeq
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8                  as BS
import           Data.Dynamic
import qualified Data.HashMap.Strict                    as HMap
import           Data.Hashable
import           Data.List.Extra                        (foldl', partition,
                                                         takeEnd)
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map
import           Data.Maybe
import qualified Data.Set                               as Set
import qualified Data.SortedList                        as SL
import qualified Data.Text                              as T
import           Data.Time
import           Data.Traversable
import           Data.Tuple.Extra
import           Data.Typeable
import           Data.Unique
import           Data.Vector                            (Vector)
import qualified Data.Vector                            as Vector
import           Development.IDE.Core.Debouncer
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.ProgressReporting
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Tracing
import           Development.IDE.GHC.Compat             (NameCacheUpdater (..),
                                                         upNameCache)
import           Development.IDE.GHC.Orphans            ()
import           Development.IDE.Graph                  hiding (ShakeValue)
import qualified Development.IDE.Graph                  as Shake
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Database
import           Development.IDE.Graph.Rule
import           Development.IDE.Types.Action
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Exports
import           Development.IDE.Types.KnownTargets
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger           hiding (Priority)
import qualified Development.IDE.Types.Logger           as Logger
import           Development.IDE.Types.Options
import           Development.IDE.Types.Shake
import           GHC.Generics
import           Language.LSP.Diagnostics
import qualified Language.LSP.Server                    as LSP
import           Language.LSP.Types
import qualified Language.LSP.Types                     as LSP
import           Language.LSP.VFS
import           System.FilePath                        hiding (makeRelative)
import           System.Time.Extra

import           Data.IORef
import           GHC.Fingerprint
import           Language.LSP.Types.Capabilities
import           NameCache
import           OpenTelemetry.Eventlog
import           PrelInfo
import           UniqSupply

import           Control.Exception.Extra                hiding (bracket_)
import qualified Data.ByteString.Char8                  as BS8
import           Data.Default
import           Data.Foldable                          (toList)
import           Data.HashSet                           (HashSet)
import qualified Data.HashSet                           as HSet
import           Data.IORef.Extra                       (atomicModifyIORef'_,
                                                         atomicModifyIORef_)
import           Data.Text                              (pack)
import qualified Development.IDE.Types.Exports          as ExportsMap
import           HieDb.Types
import           Ide.Plugin.Config
import qualified Ide.PluginUtils                        as HLS
import           Ide.Types                              (PluginId)

-- | We need to serialize writes to the database, so we send any function that
-- needs to write to the database over the channel, where it will be picked up by
-- a worker thread.
data HieDbWriter
  = HieDbWriter
  { indexQueue         :: IndexQueue
  , indexPending       :: TVar (HMap.HashMap NormalizedFilePath Fingerprint) -- ^ Avoid unnecessary/out of date indexing
  , indexCompleted     :: TVar Int -- ^ to report progress
  , indexProgressToken :: Var (Maybe LSP.ProgressToken)
  -- ^ This is a Var instead of a TVar since we need to do IO to initialise/update, so we need a lock
  }

-- | Actions to queue up on the index worker thread
type IndexQueue = TQueue (HieDb -> IO ())

-- information we stash inside the shakeExtra field
data ShakeExtras = ShakeExtras
    { --eventer :: LSP.FromServerMessage -> IO ()
     lspEnv :: Maybe (LSP.LanguageContextEnv Config)
    ,debouncer :: Debouncer NormalizedUri
    ,logger :: Logger
    ,globals :: Var (HMap.HashMap TypeRep Dynamic)
    ,state :: Var Values
    ,diagnostics :: Var DiagnosticStore
    ,hiddenDiagnostics :: Var DiagnosticStore
    ,publishedDiagnostics :: Var (HMap.HashMap NormalizedUri [Diagnostic])
    -- ^ This represents the set of diagnostics that we have published.
    -- Due to debouncing not every change might get published.
    ,positionMapping :: Var (HMap.HashMap NormalizedUri (Map TextDocumentVersion (PositionDelta, PositionMapping)))
    -- ^ Map from a text document version to a PositionMapping that describes how to map
    -- positions in a version of that document to positions in the latest version
    -- First mapping is delta from previous version and second one is an
    -- accumlation of all previous mappings.
    ,progress :: ProgressReporting
    ,ideTesting :: IdeTesting
    -- ^ Whether to enable additional lsp messages used by the test suite for checking invariants
    ,restartShakeSession
        :: [DelayedAction ()]
        -> IO ()
    ,ideNc :: IORef NameCache
    -- | A mapping of module name to known target (or candidate targets, if missing)
    ,knownTargetsVar :: Var (Hashed KnownTargets)
    -- | A mapping of exported identifiers for local modules. Updated on kick
    ,exportsMap :: Var ExportsMap
    -- | A work queue for actions added via 'runInShakeSession'
    ,actionQueue :: ActionQueue
    ,clientCapabilities :: ClientCapabilities
    , hiedb :: HieDb -- ^ Use only to read.
    , hiedbWriter :: HieDbWriter -- ^ use to write
    , persistentKeys :: Var (HMap.HashMap Key GetStalePersistent)
      -- ^ Registery for functions that compute/get "stale" results for the rule
      -- (possibly from disk)
    , vfs :: VFSHandle
    , defaultConfig :: Config
      -- ^ Default HLS config, only relevant if the client does not provide any Config
    , dirtyKeys :: IORef (HashSet SomeShakeValue)
      -- ^ Set of dirty rule keys since the last Shake run
    }

type WithProgressFunc = forall a.
    T.Text -> LSP.ProgressCancellable -> ((LSP.ProgressAmount -> IO ()) -> IO a) -> IO a
type WithIndefiniteProgressFunc = forall a.
    T.Text -> LSP.ProgressCancellable -> IO a -> IO a

type GetStalePersistent = NormalizedFilePath -> IdeAction (Maybe (Dynamic,PositionDelta,TextDocumentVersion))

getShakeExtras :: Action ShakeExtras
getShakeExtras = do
    Just x <- getShakeExtra @ShakeExtras
    return x

getShakeExtrasRules :: Rules ShakeExtras
getShakeExtrasRules = do
    Just x <- getShakeExtraRules @ShakeExtras
    return x

getPluginConfig
    :: LSP.MonadLsp Config m => PluginId -> m PluginConfig
getPluginConfig plugin = do
    config <- HLS.getClientConfig
    return $ HLS.configForPlugin config plugin

-- | Register a function that will be called to get the "stale" result of a rule, possibly from disk
-- This is called when we don't already have a result, or computing the rule failed.
-- The result of this function will always be marked as 'stale', and a 'proper' rebuild of the rule will
-- be queued if the rule hasn't run before.
addPersistentRule :: IdeRule k v => k -> (NormalizedFilePath -> IdeAction (Maybe (v,PositionDelta,TextDocumentVersion))) -> Rules ()
addPersistentRule k getVal = do
  ShakeExtras{persistentKeys} <- getShakeExtrasRules
  void $ liftIO $ modifyVar' persistentKeys $ HMap.insert (Key k) (fmap (fmap (first3 toDyn)) . getVal)

class Typeable a => IsIdeGlobal a where


-- | haskell-lsp manages the VFS internally and automatically so we cannot use
-- the builtin VFS without spawning up an LSP server. To be able to test things
-- like `setBufferModified` we abstract over the VFS implementation.
data VFSHandle = VFSHandle
    { getVirtualFile         :: NormalizedUri -> IO (Maybe VirtualFile)
        -- ^ get the contents of a virtual file
    , setVirtualFileContents :: Maybe (NormalizedUri -> Maybe T.Text -> IO ())
        -- ^ set a specific file to a value. If Nothing then we are ignoring these
        --   signals anyway so can just say something was modified
    }
instance IsIdeGlobal VFSHandle

addIdeGlobal :: IsIdeGlobal a => a -> Rules ()
addIdeGlobal x = do
    extras <- getShakeExtrasRules
    liftIO $ addIdeGlobalExtras extras x

addIdeGlobalExtras :: IsIdeGlobal a => ShakeExtras -> a -> IO ()
addIdeGlobalExtras ShakeExtras{globals} x@(typeOf -> ty) =
    void $ liftIO $ modifyVarIO' globals $ \mp -> case HMap.lookup ty mp of
        Just _ -> errorIO $ "Internal error, addIdeGlobalExtras, got the same type twice for " ++ show ty
        Nothing -> return $! HMap.insert ty (toDyn x) mp


getIdeGlobalExtras :: forall a . IsIdeGlobal a => ShakeExtras -> IO a
getIdeGlobalExtras ShakeExtras{globals} = do
    let typ = typeRep (Proxy :: Proxy a)
    x <- HMap.lookup (typeRep (Proxy :: Proxy a)) <$> readVar globals
    case x of
        Just x
            | Just x <- fromDynamic x -> pure x
            | otherwise -> errorIO $ "Internal error, getIdeGlobalExtras, wrong type for " ++ show typ ++ " (got " ++ show (dynTypeRep x) ++ ")"
        Nothing -> errorIO $ "Internal error, getIdeGlobalExtras, no entry for " ++ show typ

getIdeGlobalAction :: forall a . IsIdeGlobal a => Action a
getIdeGlobalAction = liftIO . getIdeGlobalExtras =<< getShakeExtras

getIdeGlobalState :: forall a . IsIdeGlobal a => IdeState -> IO a
getIdeGlobalState = getIdeGlobalExtras . shakeExtras


newtype GlobalIdeOptions = GlobalIdeOptions IdeOptions
instance IsIdeGlobal GlobalIdeOptions

getIdeOptions :: Action IdeOptions
getIdeOptions = do
    GlobalIdeOptions x <- getIdeGlobalAction
    return x

getIdeOptionsIO :: ShakeExtras -> IO IdeOptions
getIdeOptionsIO ide = do
    GlobalIdeOptions x <- getIdeGlobalExtras ide
    return x

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValueIO :: IdeRule k v => ShakeExtras -> k -> NormalizedFilePath -> IO (Maybe (v, PositionMapping))
lastValueIO s@ShakeExtras{positionMapping,persistentKeys,state} k file = do
    hm <- readVar state
    allMappings <- readVar positionMapping

    let readPersistent
          | IdeTesting testing <- ideTesting s -- Don't read stale persistent values in tests
          , testing = pure Nothing
          | otherwise = do
          pmap <- readVar persistentKeys
          mv <- runMaybeT $ do
            liftIO $ Logger.logDebug (logger s) $ T.pack $ "LOOKUP UP PERSISTENT FOR: " ++ show k
            f <- MaybeT $ pure $ HMap.lookup (Key k) pmap
            (dv,del,ver) <- MaybeT $ runIdeAction "lastValueIO" s $ f file
            MaybeT $ pure $ (,del,ver) <$> fromDynamic dv
          case mv of
            Nothing -> do
                void $ modifyVar' state $ HMap.alter (alterValue $ Failed True) (file,Key k)
                return Nothing
            Just (v,del,ver) -> do
                void $ modifyVar' state $ HMap.alter (alterValue $ Stale (Just del) ver (toDyn v)) (file,Key k)
                return $ Just (v,addDelta del $ mappingForVersion allMappings file ver)

        -- We got a new stale value from the persistent rule, insert it in the map without affecting diagnostics
        alterValue new Nothing = Just (ValueWithDiagnostics new mempty) -- If it wasn't in the map, give it empty diagnostics
        alterValue new (Just old@(ValueWithDiagnostics val diags)) = Just $ case val of
          -- Old failed, we can update it preserving diagnostics
          Failed{} -> ValueWithDiagnostics new diags
          -- Something already succeeded before, leave it alone
          _        -> old

    case HMap.lookup (file,Key k) hm of
      Nothing -> readPersistent
      Just (ValueWithDiagnostics v _) -> case v of
        Succeeded ver (fromDynamic -> Just v) -> pure (Just (v, mappingForVersion allMappings file ver))
        Stale del ver (fromDynamic -> Just v) -> pure (Just (v, maybe id addDelta del $ mappingForVersion allMappings file ver))
        Failed p | not p -> readPersistent
        _ -> pure Nothing

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValue :: IdeRule k v => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
lastValue key file = do
    s <- getShakeExtras
    liftIO $ lastValueIO s key file

valueVersion :: Value v -> Maybe TextDocumentVersion
valueVersion = \case
    Succeeded ver _ -> Just ver
    Stale _ ver _   -> Just ver
    Failed _        -> Nothing

mappingForVersion
    :: HMap.HashMap NormalizedUri (Map TextDocumentVersion (a, PositionMapping))
    -> NormalizedFilePath
    -> TextDocumentVersion
    -> PositionMapping
mappingForVersion allMappings file ver =
    maybe zeroMapping snd $
    Map.lookup ver =<<
    HMap.lookup (filePathToUri' file) allMappings

type IdeRule k v =
  ( Shake.RuleResult k ~ v
  , Shake.ShakeValue k
  , Show v
  , Typeable v
  , NFData v
  )

-- | A live Shake session with the ability to enqueue Actions for running.
--   Keeps the 'ShakeDatabase' open, so at most one 'ShakeSession' per database.
newtype ShakeSession = ShakeSession
  { cancelShakeSession :: IO ()
    -- ^ Closes the Shake session
  }

-- | A Shake database plus persistent store. Can be thought of as storing
--   mappings from @(FilePath, k)@ to @RuleResult k@.
data IdeState = IdeState
    {shakeDb              :: ShakeDatabase
    ,shakeSession         :: MVar ShakeSession
    ,shakeClose           :: IO ()
    ,shakeExtras          :: ShakeExtras
    ,shakeDatabaseProfile :: ShakeDatabase -> IO (Maybe FilePath)
    }



-- This is debugging code that generates a series of profiles, if the Boolean is true
shakeDatabaseProfileIO :: Maybe FilePath -> IO(ShakeDatabase -> IO (Maybe FilePath))
shakeDatabaseProfileIO mbProfileDir = do
    profileStartTime <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime
    profileCounter <- newVar (0::Int)
    return $ \shakeDb ->
        for mbProfileDir $ \dir -> do
                count <- modifyVar profileCounter $ \x -> let !y = x+1 in return (y,y)
                let file = "ide-" ++ profileStartTime ++ "-" ++ takeEnd 5 ("0000" ++ show count) <.> "html"
                shakeProfileDatabase shakeDb $ dir </> file
                return (dir </> file)

setValues :: IdeRule k v
          => Var Values
          -> k
          -> NormalizedFilePath
          -> Value v
          -> Vector FileDiagnostic
          -> IO ()
setValues state key file val diags =
    void $ modifyVar' state $ HMap.insert (file, Key key) (ValueWithDiagnostics (fmap toDyn val) diags)


-- | Delete the value stored for a given ide build key
deleteValue
  :: Shake.ShakeValue k
  => ShakeExtras
  -> k
  -> NormalizedFilePath
  -> IO ()
deleteValue ShakeExtras{dirtyKeys, state} key file = do
    void $ modifyVar' state $ HMap.delete (file, Key key)
    atomicModifyIORef_ dirtyKeys $ HSet.insert (toKey key file)

recordDirtyKeys
  :: Shake.ShakeValue k
  => ShakeExtras
  -> k
  -> [NormalizedFilePath]
  -> IO ()
recordDirtyKeys ShakeExtras{dirtyKeys} key file =
    atomicModifyIORef_ dirtyKeys $ \x -> foldl' (flip HSet.insert) x (toKey key <$> file)

-- | We return Nothing if the rule has not run and Just Failed if it has failed to produce a value.
getValues ::
  forall k v.
  IdeRule k v =>
  Var Values ->
  k ->
  NormalizedFilePath ->
  IO (Maybe (Value v, Vector FileDiagnostic))
getValues state key file = do
    vs <- readVar state
    case HMap.lookup (file, Key key) vs of
        Nothing -> pure Nothing
        Just (ValueWithDiagnostics v diagsV) -> do
            let r = fmap (fromJust . fromDynamic @v) v
            -- Force to make sure we do not retain a reference to the HashMap
            -- and we blow up immediately if the fromJust should fail
            -- (which would be an internal error).
            evaluate (r `seqValue` Just (r, diagsV))

-- | Get all the files in the project
knownTargets :: Action (Hashed KnownTargets)
knownTargets = do
  ShakeExtras{knownTargetsVar} <- getShakeExtras
  liftIO $ readVar knownTargetsVar

-- | Seq the result stored in the Shake value. This only
-- evaluates the value to WHNF not NF. We take care of the latter
-- elsewhere and doing it twice is expensive.
seqValue :: Value v -> b -> b
seqValue v b = case v of
    Succeeded ver v -> rnf ver `seq` v `seq` b
    Stale d ver v   -> rnf d `seq` rnf ver `seq` v `seq` b
    Failed _        -> b

-- | Open a 'IdeState', should be shut using 'shakeShut'.
shakeOpen :: Maybe (LSP.LanguageContextEnv Config)
          -> Config
          -> Logger
          -> Debouncer NormalizedUri
          -> Maybe FilePath
          -> IdeReportProgress
          -> IdeTesting
          -> HieDb
          -> IndexQueue
          -> VFSHandle
          -> ShakeOptions
          -> Rules ()
          -> IO IdeState
shakeOpen lspEnv defaultConfig logger debouncer
  shakeProfileDir (IdeReportProgress reportProgress) ideTesting@(IdeTesting testing) hiedb indexQueue vfs opts rules = mdo

    us <- mkSplitUniqSupply 'r'
    ideNc <- newIORef (initNameCache us knownKeyNames)
    shakeExtras <- do
        globals <- newVar HMap.empty
        state <- newVar HMap.empty
        diagnostics <- newVar mempty
        hiddenDiagnostics <- newVar mempty
        publishedDiagnostics <- newVar mempty
        positionMapping <- newVar HMap.empty
        knownTargetsVar <- newVar $ hashed HMap.empty
        let restartShakeSession = shakeRestart ideState
        persistentKeys <- newVar HMap.empty
        indexPending <- newTVarIO HMap.empty
        indexCompleted <- newTVarIO 0
        indexProgressToken <- newVar Nothing
        let hiedbWriter = HieDbWriter{..}
        exportsMap <- newVar mempty
        -- lazily initialize the exports map with the contents of the hiedb
        _ <- async $ do
            logDebug logger "Initializing exports map from hiedb"
            em <- createExportsMapHieDb hiedb
            modifyVar' exportsMap (<> em)
            logDebug logger $ "Done initializing exports map from hiedb (" <> pack(show (ExportsMap.size em)) <> ")"

        progress <- do
            let (before, after) = if testing then (0,0.1) else (0.1,0.1)
            if reportProgress
                then delayedProgressReporting before after lspEnv optProgressStyle
                else noProgressReporting
        actionQueue <- newQueue

        let clientCapabilities = maybe def LSP.resClientCapabilities lspEnv

        dirtyKeys <- newIORef mempty
        pure ShakeExtras{..}
    (shakeDbM, shakeClose) <-
        shakeOpenDatabase
            opts { shakeExtra = newShakeExtra shakeExtras }
            rules
    shakeDb <- shakeDbM
    shakeSession <- newEmptyMVar
    shakeDatabaseProfile <- shakeDatabaseProfileIO shakeProfileDir
    let ideState = IdeState{..}

    IdeOptions
        { optOTMemoryProfiling = IdeOTMemoryProfiling otProfilingEnabled
        , optProgressStyle
        } <- getIdeOptionsIO shakeExtras
    startTelemetry otProfilingEnabled logger $ state shakeExtras

    return ideState

-- | Must be called in the 'Initialized' handler and only once
shakeSessionInit :: IdeState -> IO ()
shakeSessionInit IdeState{..} = do
    initSession <- newSession shakeExtras shakeDb []
    putMVar shakeSession initSession

shakeProfile :: IdeState -> FilePath -> IO ()
shakeProfile IdeState{..} = shakeProfileDatabase shakeDb

shakeShut :: IdeState -> IO ()
shakeShut IdeState{..} = withMVar shakeSession $ \runner -> do
    -- Shake gets unhappy if you try to close when there is a running
    -- request so we first abort that.
    void $ cancelShakeSession runner
    shakeClose
    progressStop $ progress shakeExtras


-- | This is a variant of withMVar where the first argument is run unmasked and if it throws
-- an exception, the previous value is restored while the second argument is executed masked.
withMVar' :: MVar a -> (a -> IO b) -> (b -> IO (a, c)) -> IO c
withMVar' var unmasked masked = uninterruptibleMask $ \restore -> do
    a <- takeMVar var
    b <- restore (unmasked a) `onException` putMVar var a
    (a', c) <- masked b
    putMVar var a'
    pure c


mkDelayedAction :: String -> Logger.Priority -> Action a -> DelayedAction a
mkDelayedAction = DelayedAction Nothing

-- | These actions are run asynchronously after the current action is
-- finished running. For example, to trigger a key build after a rule
-- has already finished as is the case with useWithStaleFast
delayedAction :: DelayedAction a -> IdeAction (IO a)
delayedAction a = do
  extras <- ask
  liftIO $ shakeEnqueue extras a

-- | Restart the current 'ShakeSession' with the given system actions.
--   Any actions running in the current session will be aborted,
--   but actions added via 'shakeEnqueue' will be requeued.
shakeRestart :: IdeState -> [DelayedAction ()] -> IO ()
shakeRestart IdeState{..} acts =
    withMVar'
        shakeSession
        (\runner -> do
              (stopTime,()) <- duration (cancelShakeSession runner)
              res <- shakeDatabaseProfile shakeDb
              backlog <- readIORef $ dirtyKeys shakeExtras
              let profile = case res of
                      Just fp -> ", profile saved at " <> fp
                      _       -> ""
              let msg = T.pack $ "Restarting build session " ++ keysMsg ++ abortMsg
                  keysMsg = "for keys " ++ show (HSet.toList backlog) ++ " "
                  abortMsg = "(aborting the previous one took " ++ showDuration stopTime ++ profile ++ ")"
              logDebug (logger shakeExtras) msg
              notifyTestingLogMessage shakeExtras msg
        )
        -- It is crucial to be masked here, otherwise we can get killed
        -- between spawning the new thread and updating shakeSession.
        -- See https://github.com/haskell/ghcide/issues/79
        (\() -> do
          (,()) <$> newSession shakeExtras shakeDb acts)

notifyTestingLogMessage :: ShakeExtras -> T.Text -> IO ()
notifyTestingLogMessage extras msg = do
    (IdeTesting isTestMode) <- optTesting <$> getIdeOptionsIO extras
    let notif = LSP.LogMessageParams LSP.MtLog msg
    when isTestMode $ mRunLspT (lspEnv extras) $ LSP.sendNotification LSP.SWindowLogMessage notif


-- | Enqueue an action in the existing 'ShakeSession'.
--   Returns a computation to block until the action is run, propagating exceptions.
--   Assumes a 'ShakeSession' is available.
--
--   Appropriate for user actions other than edits.
shakeEnqueue :: ShakeExtras -> DelayedAction a -> IO (IO a)
shakeEnqueue ShakeExtras{actionQueue, logger} act = do
    (b, dai) <- instantiateDelayedAction act
    atomically $ pushQueue dai actionQueue
    let wait' b =
            waitBarrier b `catches`
              [ Handler(\BlockedIndefinitelyOnMVar ->
                    fail $ "internal bug: forever blocked on MVar for " <>
                            actionName act)
              , Handler (\e@AsyncCancelled -> do
                  logPriority logger Debug $ T.pack $ actionName act <> " was cancelled"

                  atomically $ abortQueue dai actionQueue
                  throw e)
              ]
    return (wait' b >>= either throwIO return)

-- | Set up a new 'ShakeSession' with a set of initial actions
--   Will crash if there is an existing 'ShakeSession' running.
newSession
    :: ShakeExtras
    -> ShakeDatabase
    -> [DelayedActionInternal]
    -> IO ShakeSession
newSession extras@ShakeExtras{..} shakeDb acts = do
    IdeOptions{optRunSubset} <- getIdeOptionsIO extras
    reenqueued <- atomically $ peekInProgress actionQueue
    allPendingKeys <-
        if optRunSubset
          then Just <$> readIORef dirtyKeys
          else return Nothing
    let
        -- A daemon-like action used to inject additional work
        -- Runs actions from the work queue sequentially
        pumpActionThread otSpan = do
            d <- liftIO $ atomically $ popQueue actionQueue
            void $ parallel [run otSpan d, pumpActionThread otSpan]

        -- TODO figure out how to thread the otSpan into defineEarlyCutoff
        run _otSpan d  = do
            start <- liftIO offsetTime
            getAction d
            liftIO $ atomically $ doneQueue d actionQueue
            runTime <- liftIO start
            let msg = T.pack $ "finish: " ++ actionName d
                            ++ " (took " ++ showDuration runTime ++ ")"
            liftIO $ do
                logPriority logger (actionPriority d) msg
                notifyTestingLogMessage extras msg

        -- The inferred type signature doesn't work in ghc >= 9.0.1
        workRun :: (forall b. IO b -> IO b) -> IO (IO ())
        workRun restore = withSpan "Shake session" $ \otSpan -> do
          whenJust allPendingKeys $ \kk -> setTag otSpan "keys" (BS8.pack $ unlines $ map show $ toList kk)
          let keysActs = pumpActionThread otSpan : map (run otSpan) (reenqueued ++ acts)
          res <- try @SomeException $
            restore $ shakeRunDatabaseForKeys (HSet.toList <$> allPendingKeys) shakeDb keysActs
          let res' = case res of
                      Left e  -> "exception: " <> displayException e
                      Right _ -> "completed"
          let msg = T.pack $ "Finishing build session(" ++ res' ++ ")"
          return $ do
              logDebug logger msg
              notifyTestingLogMessage extras msg

    -- Do the work in a background thread
    workThread <- asyncWithUnmask workRun

    -- run the wrap up in a separate thread since it contains interruptible
    -- commands (and we are not using uninterruptible mask)
    _ <- async $ join $ wait workThread

    --  Cancelling is required to flush the Shake database when either
    --  the filesystem or the Ghc configuration have changed
    let cancelShakeSession :: IO ()
        cancelShakeSession = cancel workThread

    pure (ShakeSession{..})

instantiateDelayedAction
    :: DelayedAction a
    -> IO (Barrier (Either SomeException a), DelayedActionInternal)
instantiateDelayedAction (DelayedAction _ s p a) = do
  u <- newUnique
  b <- newBarrier
  let a' = do
        -- work gets reenqueued when the Shake session is restarted
        -- it can happen that a work item finished just as it was reenqueud
        -- in that case, skipping the work is fine
        alreadyDone <- liftIO $ isJust <$> waitBarrierMaybe b
        unless alreadyDone $ do
          x <- actionCatch @SomeException (Right <$> a) (pure . Left)
          -- ignore exceptions if the barrier has been filled concurrently
          liftIO $ void $ try @SomeException $ signalBarrier b x
      d' = DelayedAction (Just u) s p a'
  return (b, d')

getDiagnostics :: IdeState -> IO [FileDiagnostic]
getDiagnostics IdeState{shakeExtras = ShakeExtras{diagnostics}} = do
    val <- readVar diagnostics
    return $ getAllDiagnostics val

getHiddenDiagnostics :: IdeState -> IO [FileDiagnostic]
getHiddenDiagnostics IdeState{shakeExtras = ShakeExtras{hiddenDiagnostics}} = do
    val <- readVar hiddenDiagnostics
    return $ getAllDiagnostics val

-- | Clear the results for all files that do not match the given predicate.
garbageCollect :: (NormalizedFilePath -> Bool) -> Action ()
garbageCollect keep = do
    ShakeExtras{state, diagnostics,hiddenDiagnostics,publishedDiagnostics,positionMapping} <- getShakeExtras
    liftIO $
        do newState <- modifyVar' state $ HMap.filterWithKey (\(file, _) _ -> keep file)
           void $ modifyVar' diagnostics $ filterDiagnostics keep
           void $ modifyVar' hiddenDiagnostics $ filterDiagnostics keep
           void $ modifyVar' publishedDiagnostics $ HMap.filterWithKey (\uri _ -> keep (fromUri uri))
           let versionsForFile =
                   HMap.fromListWith Set.union $
                   mapMaybe (\((file, _key), ValueWithDiagnostics v _) -> (filePathToUri' file,) . Set.singleton <$> valueVersion v) $
                   HMap.toList newState
           void $ modifyVar' positionMapping $ filterVersionMap versionsForFile

-- | Define a new Rule without early cutoff
define
    :: IdeRule k v
    => (k -> NormalizedFilePath -> Action (IdeResult v)) -> Rules ()
define op = defineEarlyCutoff $ Rule $ \k v -> (Nothing,) <$> op k v

defineNoDiagnostics
    :: IdeRule k v
    => (k -> NormalizedFilePath -> Action (Maybe v)) -> Rules ()
defineNoDiagnostics op = defineEarlyCutoff $ RuleNoDiagnostics $ \k v -> (Nothing,) <$> op k v

-- | Request a Rule result if available
use :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe v)
use key file = head <$> uses key [file]

-- | Request a Rule result, it not available return the last computed result, if any, which may be stale
useWithStale :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
useWithStale key file = head <$> usesWithStale key [file]

-- | Request a Rule result, it not available return the last computed result which may be stale.
--   Errors out if none available.
useWithStale_ :: IdeRule k v
    => k -> NormalizedFilePath -> Action (v, PositionMapping)
useWithStale_ key file = head <$> usesWithStale_ key [file]

-- | Plural version of 'useWithStale_'
usesWithStale_ :: IdeRule k v => k -> [NormalizedFilePath] -> Action [(v, PositionMapping)]
usesWithStale_ key files = do
    res <- usesWithStale key files
    case sequence res of
        Nothing -> liftIO $ throwIO $ BadDependency (show key)
        Just v  -> return v

-- | IdeActions are used when we want to return a result immediately, even if it
-- is stale Useful for UI actions like hover, completion where we don't want to
-- block.
--
-- Run via 'runIdeAction'.
newtype IdeAction a = IdeAction { runIdeActionT  :: (ReaderT ShakeExtras IO) a }
    deriving newtype (MonadReader ShakeExtras, MonadIO, Functor, Applicative, Monad)

runIdeAction :: String -> ShakeExtras -> IdeAction a -> IO a
runIdeAction _herald s i = runReaderT (runIdeActionT i) s

askShake :: IdeAction ShakeExtras
askShake = ask

mkUpdater :: IORef NameCache -> NameCacheUpdater
mkUpdater ref = NCU (upNameCache ref)

-- | A (maybe) stale result now, and an up to date one later
data FastResult a = FastResult { stale :: Maybe (a,PositionMapping), uptoDate :: IO (Maybe a)  }

-- | Lookup value in the database and return with the stale value immediately
-- Will queue an action to refresh the value.
-- Might block the first time the rule runs, but never blocks after that.
useWithStaleFast :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (Maybe (v, PositionMapping))
useWithStaleFast key file = stale <$> useWithStaleFast' key file

-- | Same as useWithStaleFast but lets you wait for an up to date result
useWithStaleFast' :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (FastResult v)
useWithStaleFast' key file = do
  -- This lookup directly looks up the key in the shake database and
  -- returns the last value that was computed for this key without
  -- checking freshness.

  -- Async trigger the key to be built anyway because we want to
  -- keep updating the value in the key.
  wait <- delayedAction $ mkDelayedAction ("C:" ++ show key) Debug $ use key file

  s@ShakeExtras{state} <- askShake
  r <- liftIO $ getValues state key file
  liftIO $ case r of
    -- block for the result if we haven't computed before
    Nothing -> do
      -- Check if we can get a stale value from disk
      res <- lastValueIO s key file
      case res of
        Nothing -> do
          a <- wait
          pure $ FastResult ((,zeroMapping) <$> a) (pure a)
        Just _ -> pure $ FastResult res wait
    -- Otherwise, use the computed value even if it's out of date.
    Just _ -> do
      res <- lastValueIO s key file
      pure $ FastResult res wait

useNoFile :: IdeRule k v => k -> Action (Maybe v)
useNoFile key = use key emptyFilePath

use_ :: IdeRule k v => k -> NormalizedFilePath -> Action v
use_ key file = head <$> uses_ key [file]

useNoFile_ :: IdeRule k v => k -> Action v
useNoFile_ key = use_ key emptyFilePath

uses_ :: IdeRule k v => k -> [NormalizedFilePath] -> Action [v]
uses_ key files = do
    res <- uses key files
    case sequence res of
        Nothing -> liftIO $ throwIO $ BadDependency (show key)
        Just v  -> return v

-- | Plural version of 'use'
uses :: IdeRule k v
    => k -> [NormalizedFilePath] -> Action [Maybe v]
uses key files = map (\(A value) -> currentValue value) <$> apply (map (Q . (key,)) files)

-- | Return the last computed result which might be stale.
usesWithStale :: IdeRule k v
    => k -> [NormalizedFilePath] -> Action [Maybe (v, PositionMapping)]
usesWithStale key files = do
    _ <- apply (map (Q . (key,)) files)
    -- We don't look at the result of the 'apply' since 'lastValue' will
    -- return the most recent successfully computed value regardless of
    -- whether the rule succeeded or not.
    mapM (lastValue key) files

data RuleBody k v
  = Rule (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, IdeResult v))
  | RuleNoDiagnostics (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe v))


-- | Define a new Rule with early cutoff
defineEarlyCutoff
    :: IdeRule k v
    => RuleBody k v
    -> Rules ()
defineEarlyCutoff (Rule op) = addRule $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode -> otTracedAction key file mode isSuccess $ do
    defineEarlyCutoff' True key file old mode $ op key file
defineEarlyCutoff (RuleNoDiagnostics op) = addRule $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode -> otTracedAction key file mode isSuccess $ do
    defineEarlyCutoff' False key file old mode $ second (mempty,) <$> op key file

defineNoFile :: IdeRule k v => (k -> Action v) -> Rules ()
defineNoFile f = defineNoDiagnostics $ \k file -> do
    if file == emptyFilePath then do res <- f k; return (Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"

defineEarlyCutOffNoFile :: IdeRule k v => (k -> Action (BS.ByteString, v)) -> Rules ()
defineEarlyCutOffNoFile f = defineEarlyCutoff $ RuleNoDiagnostics $ \k file -> do
    if file == emptyFilePath then do (hash, res) <- f k; return (Just hash, Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"

defineEarlyCutoff'
    :: IdeRule k v
    => Bool  -- ^ update diagnostics
    -> k
    -> NormalizedFilePath
    -> Maybe BS.ByteString
    -> RunMode
    -> Action (Maybe BS.ByteString, IdeResult v)
    -> Action (RunResult (A (RuleResult k)))
defineEarlyCutoff' doDiagnostics key file old mode action = do
    extras@ShakeExtras{state, progress, logger, dirtyKeys} <- getShakeExtras
    options <- getIdeOptions
    (if optSkipProgress options key then id else inProgress progress file) $ do
        val <- case old of
            Just old | mode == RunDependenciesSame -> do
                v <- liftIO $ getValues state key file
                case v of
                    -- No changes in the dependencies and we have
                    -- an existing result.
                    Just (v, diags) -> do
                        when doDiagnostics $
                            updateFileDiagnostics file (Key key) extras $ map (\(_,y,z) -> (y,z)) $ Vector.toList diags
                        return $ Just $ RunResult ChangedNothing old $ A v
                    _ -> return Nothing
            _ -> return Nothing
        res <- case val of
            Just res -> return res
            Nothing -> do
                (bs, (diags, res)) <- actionCatch
                    (do v <- action; liftIO $ evaluate $ force v) $
                    \(e :: SomeException) -> pure (Nothing, ([ideErrorText file $ T.pack $ show e | not $ isBadDependency e],Nothing))
                modTime <- liftIO $ (currentValue . fst =<<) <$> getValues state GetModificationTime file
                (bs, res) <- case res of
                    Nothing -> do
                        staleV <- liftIO $ getValues state key file
                        pure $ case staleV of
                            Nothing -> (toShakeValue ShakeResult bs, Failed False)
                            Just v -> case v of
                                (Succeeded ver v, _) ->
                                    (toShakeValue ShakeStale bs, Stale Nothing ver v)
                                (Stale d ver v, _) ->
                                    (toShakeValue ShakeStale bs, Stale d ver v)
                                (Failed b, _) ->
                                    (toShakeValue ShakeResult bs, Failed b)
                    Just v -> pure (maybe ShakeNoCutoff ShakeResult bs, Succeeded (vfsVersion =<< modTime) v)
                liftIO $ setValues state key file res (Vector.fromList diags)
                if doDiagnostics
                    then updateFileDiagnostics file (Key key) extras $ map (\(_,y,z) -> (y,z)) diags
                    else forM_ diags $ \d -> liftIO $ logWarning logger $ showDiagnosticsColored [d]
                let eq = case (bs, fmap decodeShakeValue old) of
                        (ShakeResult a, Just (ShakeResult b)) -> a == b
                        (ShakeStale a, Just (ShakeStale b))   -> a == b
                        -- If we do not have a previous result
                        -- or we got ShakeNoCutoff we always return False.
                        _                                     -> False
                return $ RunResult
                    (if eq then ChangedRecomputeSame else ChangedRecomputeDiff)
                    (encodeShakeValue bs) $
                    A res
        liftIO $ atomicModifyIORef'_ dirtyKeys (HSet.delete $ toKey key file)
        return res

isSuccess :: A v -> Bool
isSuccess (A Failed{}) = False
isSuccess _            = True

-- | Rule type, input file
data QDisk k = QDisk k NormalizedFilePath
  deriving (Eq, Generic)

instance Hashable k => Hashable (QDisk k)

instance NFData k => NFData (QDisk k)

instance Binary k => Binary (QDisk k)

instance Show k => Show (QDisk k) where
    show (QDisk k file) =
        show k ++ "; " ++ fromNormalizedFilePath file

type instance RuleResult (QDisk k) = Bool

data OnDiskRule = OnDiskRule
  { getHash :: Action BS.ByteString
  -- This is used to figure out if the state on disk corresponds to the state in the Shake
  -- database and we can therefore avoid rerunning. Often this can just be the file hash but
  -- in some cases we can be more aggressive, e.g., for GHC interface files this can be the ABI hash which
  -- is more stable than the hash of the interface file.
  -- An empty bytestring indicates that the state on disk is invalid, e.g., files are missing.
  -- We do not use a Maybe since we have to deal with encoding things into a ByteString anyway in the Shake DB.
  , runRule :: Action (IdeResult BS.ByteString)
  -- The actual rule code which produces the new hash (or Nothing if the rule failed) and the diagnostics.
  }

-- This is used by the DAML compiler for incremental builds. Right now this is not used by
-- ghcide itself but that might change in the future.
-- The reason why this code lives in ghcide and in particular in this module is that it depends quite heavily on
-- the internals of this module that we do not want to expose.
defineOnDisk
  :: (Shake.ShakeValue k, RuleResult k ~ ())
  => (k -> NormalizedFilePath -> OnDiskRule)
  -> Rules ()
defineOnDisk act = addRule $
  \(QDisk key file) (mbOld :: Maybe BS.ByteString) mode -> do
      extras <- getShakeExtras
      let OnDiskRule{..} = act key file
      let validateHash h
              | BS.null h = Nothing
              | otherwise = Just h
      let runAct = actionCatch runRule $
              \(e :: SomeException) -> pure ([ideErrorText file $ T.pack $ displayException e | not $ isBadDependency e], Nothing)
      case mbOld of
          Nothing -> do
              (diags, mbHash) <- runAct
              updateFileDiagnostics file (Key key) extras $ map (\(_,y,z) -> (y,z)) diags
              pure $ RunResult ChangedRecomputeDiff (fromMaybe "" mbHash) (isJust mbHash)
          Just old -> do
              current <- validateHash <$> (actionCatch getHash $ \(_ :: SomeException) -> pure "")
              if mode == RunDependenciesSame && Just old == current && not (BS.null old)
                  then
                    -- None of our dependencies changed, we’ve had a successful run before and
                    -- the state on disk matches the state in the Shake database.
                    pure $ RunResult ChangedNothing (fromMaybe "" current) (isJust current)
                  else do
                    (diags, mbHash) <- runAct
                    updateFileDiagnostics file (Key key) extras $ map (\(_,y,z) -> (y,z)) diags
                    let change
                          | mbHash == Just old = ChangedRecomputeSame
                          | otherwise = ChangedRecomputeDiff
                    pure $ RunResult change (fromMaybe "" mbHash) (isJust mbHash)

needOnDisk :: (Shake.ShakeValue k, RuleResult k ~ ()) => k -> NormalizedFilePath -> Action ()
needOnDisk k file = do
    successfull <- apply1 (QDisk k file)
    liftIO $ unless successfull $ throwIO $ BadDependency (show k)

needOnDisks :: (Shake.ShakeValue k, RuleResult k ~ ()) => k -> [NormalizedFilePath] -> Action ()
needOnDisks k files = do
    successfulls <- apply $ map (QDisk k) files
    liftIO $ unless (and successfulls) $ throwIO $ BadDependency (show k)

updateFileDiagnostics :: MonadIO m
  => NormalizedFilePath
  -> Key
  -> ShakeExtras
  -> [(ShowDiagnostic,Diagnostic)] -- ^ current results
  -> m ()
updateFileDiagnostics fp k ShakeExtras{logger, diagnostics, hiddenDiagnostics, publishedDiagnostics, state, debouncer, lspEnv} current = liftIO $ do
    modTime <- (currentValue . fst =<<) <$> getValues state GetModificationTime fp
    let (currentShown, currentHidden) = partition ((== ShowDiag) . fst) current
        uri = filePathToUri' fp
        ver = vfsVersion =<< modTime
        update new store =
            let store' = setStageDiagnostics uri ver (T.pack $ show k) new store
                new' = getUriDiagnostics uri store'
            in (store', new')
    mask_ $ do
        -- Mask async exceptions to ensure that updated diagnostics are always
        -- published. Otherwise, we might never publish certain diagnostics if
        -- an exception strikes between modifyVar but before
        -- publishDiagnosticsNotification.
        newDiags <- modifyVar diagnostics $ pure . update (map snd currentShown)
        _ <- modifyVar hiddenDiagnostics $  pure . update (map snd currentHidden)
        let uri = filePathToUri' fp
        let delay = if null newDiags then 0.1 else 0
        registerEvent debouncer delay uri $ do
             join $ mask_ $ modifyVar publishedDiagnostics $ \published -> do
                 let lastPublish = HMap.lookupDefault [] uri published
                     !published' = HMap.insert uri newDiags published
                     action = when (lastPublish /= newDiags) $ case lspEnv of
                        Nothing -> -- Print an LSP event.
                            logInfo logger $ showDiagnosticsColored $ map (fp,ShowDiag,) newDiags
                        Just env -> LSP.runLspT env $
                            LSP.sendNotification LSP.STextDocumentPublishDiagnostics $
                            LSP.PublishDiagnosticsParams (fromNormalizedUri uri) ver (List newDiags)
                 return (published', action)

newtype Priority = Priority Double

setPriority :: Priority -> Action ()
setPriority (Priority p) = reschedule p

ideLogger :: IdeState -> Logger
ideLogger IdeState{shakeExtras=ShakeExtras{logger}} = logger

actionLogger :: Action Logger
actionLogger = do
    ShakeExtras{logger} <- getShakeExtras
    return logger


getDiagnosticsFromStore :: StoreItem -> [Diagnostic]
getDiagnosticsFromStore (StoreItem _ diags) = concatMap SL.fromSortedList $ Map.elems diags


-- | Sets the diagnostics for a file and compilation step
--   if you want to clear the diagnostics call this with an empty list
setStageDiagnostics
    :: NormalizedUri
    -> TextDocumentVersion -- ^ the time that the file these diagnostics originate from was last edited
    -> T.Text
    -> [LSP.Diagnostic]
    -> DiagnosticStore
    -> DiagnosticStore
setStageDiagnostics uri ver stage diags ds = updateDiagnostics ds uri ver updatedDiags
  where
    updatedDiags = Map.singleton (Just stage) (SL.toSortedList diags)

getAllDiagnostics ::
    DiagnosticStore ->
    [FileDiagnostic]
getAllDiagnostics =
    concatMap (\(k,v) -> map (fromUri k,ShowDiag,) $ getDiagnosticsFromStore v) . HMap.toList

getUriDiagnostics ::
    NormalizedUri ->
    DiagnosticStore ->
    [LSP.Diagnostic]
getUriDiagnostics uri ds =
    maybe [] getDiagnosticsFromStore $
    HMap.lookup uri ds

filterDiagnostics ::
    (NormalizedFilePath -> Bool) ->
    DiagnosticStore ->
    DiagnosticStore
filterDiagnostics keep =
    HMap.filterWithKey (\uri _ -> maybe True (keep . toNormalizedFilePath') $ uriToFilePath' $ fromNormalizedUri uri)

filterVersionMap
    :: HMap.HashMap NormalizedUri (Set.Set TextDocumentVersion)
    -> HMap.HashMap NormalizedUri (Map TextDocumentVersion a)
    -> HMap.HashMap NormalizedUri (Map TextDocumentVersion a)
filterVersionMap =
    HMap.intersectionWith $ \versionsToKeep versionMap -> Map.restrictKeys versionMap versionsToKeep

updatePositionMapping :: IdeState -> VersionedTextDocumentIdentifier -> List TextDocumentContentChangeEvent -> IO ()
updatePositionMapping IdeState{shakeExtras = ShakeExtras{positionMapping}} VersionedTextDocumentIdentifier{..} (List changes) = do
    modifyVar_ positionMapping $ \allMappings -> do
        let uri = toNormalizedUri _uri
        let mappingForUri = HMap.lookupDefault Map.empty uri allMappings
        let (_, updatedMapping) =
                -- Very important to use mapAccum here so that the tails of
                -- each mapping can be shared, otherwise quadratic space is
                -- used which is evident in long running sessions.
                Map.mapAccumRWithKey (\acc _k (delta, _) -> let new = addDelta delta acc in (new, (delta, acc)))
                  zeroMapping
                  (Map.insert _version (shared_change, zeroMapping) mappingForUri)
        pure $ HMap.insert uri updatedMapping allMappings
  where
    shared_change = mkDelta changes
