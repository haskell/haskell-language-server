-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}

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
    IdeState, shakeExtras,
    ShakeExtras(..), getShakeExtras, getShakeExtrasRules,
    KnownTargets, Target(..), toKnownFiles,
    IdeRule, IdeResult,
    GetModificationTime(GetModificationTime, GetModificationTime_, missingFileDiagnostics),
    shakeOpen, shakeShut,
    shakeRestart,
    shakeEnqueue,
    shakeProfile,
    use, useNoFile, uses, useWithStaleFast, useWithStaleFast', delayedAction,
    FastResult(..),
    use_, useNoFile_, uses_,
    useWithStale, usesWithStale,
    useWithStale_, usesWithStale_,
    define, defineEarlyCutoff, defineOnDisk, needOnDisk, needOnDisks,
    getDiagnostics,
    getHiddenDiagnostics,
    IsIdeGlobal, addIdeGlobal, addIdeGlobalExtras, getIdeGlobalState, getIdeGlobalAction,
    getIdeGlobalExtras,
    getIdeOptions,
    getIdeOptionsIO,
    GlobalIdeOptions(..),
    garbageCollect,
    knownTargets,
    setPriority,
    sendEvent,
    ideLogger,
    actionLogger,
    FileVersion(..),
    Priority(..),
    updatePositionMapping,
    deleteValue,
    OnDiskRule(..),
    WithProgressFunc, WithIndefiniteProgressFunc,
    ProgressEvent(..),
    DelayedAction, mkDelayedAction,
    IdeAction(..), runIdeAction,
    mkUpdater,
    -- Exposed for testing.
    Q(..),
    ) where

import           Development.Shake hiding (ShakeValue, doesFileExist, Info)
import           Development.Shake.Database
import           Development.Shake.Classes
import           Development.Shake.Rule
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS
import           Data.Dynamic
import           Data.Maybe
import           Data.Map.Strict (Map)
import           Data.List.Extra (partition, takeEnd)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tuple.Extra
import Data.Unique
import Development.IDE.Core.Debouncer
import Development.IDE.GHC.Compat (NameCacheUpdater(..), upNameCache )
import Development.IDE.GHC.Orphans ()
import Development.IDE.Core.PositionMapping
import Development.IDE.Core.RuleTypes
import Development.IDE.Types.Action
import Development.IDE.Types.Logger hiding (Priority)
import Development.IDE.Types.KnownTargets
import Development.IDE.Types.Shake
import qualified Development.IDE.Types.Logger as Logger
import Language.Haskell.LSP.Diagnostics
import qualified Data.SortedList as SL
import           Development.IDE.Types.Diagnostics
import Development.IDE.Types.Exports
import Development.IDE.Types.Location
import Development.IDE.Types.Options
import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import           Control.Concurrent.STM (readTVar, writeTVar, newTVarIO, atomically)
import           Control.DeepSeq
import           Control.Exception.Extra
import           System.Time.Extra
import           Data.Typeable
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import           System.FilePath hiding (makeRelative)
import qualified Development.Shake as Shake
import           Control.Monad.Extra
import           Data.Time
import           GHC.Generics
import           System.IO.Unsafe
import Language.Haskell.LSP.Types
import qualified Control.Monad.STM as STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Traversable
import Data.Hashable
import Development.IDE.Core.Tracing

import Data.IORef
import NameCache
import UniqSupply
import PrelInfo
import Language.Haskell.LSP.Types.Capabilities
import OpenTelemetry.Eventlog

-- information we stash inside the shakeExtra field
data ShakeExtras = ShakeExtras
    {eventer :: LSP.FromServerMessage -> IO ()
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
    ,inProgress :: Var (HMap.HashMap NormalizedFilePath Int)
    -- ^ How many rules are running for each file
    ,progressUpdate :: ProgressEvent -> IO ()
    -- ^ The generator for unique Lsp identifiers
    ,ideTesting :: IdeTesting
    -- ^ Whether to enable additional lsp messages used by the test suite for checking invariants
    ,session :: MVar ShakeSession
    -- ^ Used in the GhcSession rule to forcefully restart the session after adding a new component
    ,withProgress           :: WithProgressFunc
    -- ^ Report progress about some long running operation (on top of the progress shown by 'lspShakeProgress')
    ,withIndefiniteProgress :: WithIndefiniteProgressFunc
    -- ^ Same as 'withProgress', but for processes that do not report the percentage complete
    ,restartShakeSession :: [DelayedAction ()] -> IO ()
    ,ideNc :: IORef NameCache
    -- | A mapping of module name to known target (or candidate targets, if missing)
    ,knownTargetsVar :: Var (Hashed KnownTargets)
    -- | A mapping of exported identifiers for local modules. Updated on kick
    ,exportsMap :: Var ExportsMap
    -- | A work queue for actions added via 'runInShakeSession'
    ,actionQueue :: ActionQueue
    ,clientCapabilities :: ClientCapabilities
    }

type WithProgressFunc = forall a.
    T.Text -> LSP.ProgressCancellable -> ((LSP.Progress -> IO ()) -> IO a) -> IO a
type WithIndefiniteProgressFunc = forall a.
    T.Text -> LSP.ProgressCancellable -> IO a -> IO a

data ProgressEvent
    = KickStarted
    | KickCompleted

getShakeExtras :: Action ShakeExtras
getShakeExtras = do
    Just x <- getShakeExtra @ShakeExtras
    return x

getShakeExtrasRules :: Rules ShakeExtras
getShakeExtrasRules = do
    Just x <- getShakeExtraRules @ShakeExtras
    return x

class Typeable a => IsIdeGlobal a where

addIdeGlobal :: IsIdeGlobal a => a -> Rules ()
addIdeGlobal x = do
    extras <- getShakeExtrasRules
    liftIO $ addIdeGlobalExtras extras x

addIdeGlobalExtras :: IsIdeGlobal a => ShakeExtras -> a -> IO ()
addIdeGlobalExtras ShakeExtras{globals} x@(typeOf -> ty) =
    liftIO $ modifyVar_ globals $ \mp -> case HMap.lookup ty mp of
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
lastValueIO :: ShakeExtras -> NormalizedFilePath -> Value v -> IO (Maybe (v, PositionMapping))
lastValueIO ShakeExtras{positionMapping} file v = do
    allMappings <- liftIO $ readVar positionMapping
    pure $ case v of
        Succeeded ver v -> Just (v, mappingForVersion allMappings file ver)
        Stale ver v -> Just (v, mappingForVersion allMappings file ver)
        Failed -> Nothing

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValue :: NormalizedFilePath -> Value v -> Action (Maybe (v, PositionMapping))
lastValue file v = do
    s <- getShakeExtras
    liftIO $ lastValueIO s file v

valueVersion :: Value v -> Maybe TextDocumentVersion
valueVersion = \case
    Succeeded ver _ -> Just ver
    Stale ver _ -> Just ver
    Failed -> Nothing

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
    {shakeDb         :: ShakeDatabase
    ,shakeSession    :: MVar ShakeSession
    ,shakeClose      :: IO ()
    ,shakeExtras     :: ShakeExtras
    ,shakeProfileDir :: Maybe FilePath
    ,stopProgressReporting :: IO ()
    }



-- This is debugging code that generates a series of profiles, if the Boolean is true
shakeDatabaseProfile :: Maybe FilePath -> ShakeDatabase -> IO (Maybe FilePath)
shakeDatabaseProfile mbProfileDir shakeDb =
        for mbProfileDir $ \dir -> do
                count <- modifyVar profileCounter $ \x -> let !y = x+1 in return (y,y)
                let file = "ide-" ++ profileStartTime ++ "-" ++ takeEnd 5 ("0000" ++ show count) <.> "html"
                shakeProfileDatabase shakeDb $ dir </> file
                return (dir </> file)

{-# NOINLINE profileStartTime #-}
profileStartTime :: String
profileStartTime = unsafePerformIO $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime

{-# NOINLINE profileCounter #-}
profileCounter :: Var Int
profileCounter = unsafePerformIO $ newVar 0

setValues :: IdeRule k v
          => Var Values
          -> k
          -> NormalizedFilePath
          -> Value v
          -> IO ()
setValues state key file val = modifyVar_ state $ \vals -> do
    -- Force to make sure the old HashMap is not retained
    evaluate $ HMap.insert (file, Key key) (fmap toDyn val) vals

-- | Delete the value stored for a given ide build key
deleteValue
  :: (Typeable k, Hashable k, Eq k, Show k)
  => IdeState
  -> k
  -> NormalizedFilePath
  -> IO ()
deleteValue IdeState{shakeExtras = ShakeExtras{state}} key file = modifyVar_ state $ \vals ->
    evaluate $ HMap.delete (file, Key key) vals

-- | We return Nothing if the rule has not run and Just Failed if it has failed to produce a value.
getValues :: forall k v. IdeRule k v => Var Values -> k -> NormalizedFilePath -> IO (Maybe (Value v))
getValues state key file = do
    vs <- readVar state
    case HMap.lookup (file, Key key) vs of
        Nothing -> pure Nothing
        Just v -> do
            let r = fmap (fromJust . fromDynamic @v) v
            -- Force to make sure we do not retain a reference to the HashMap
            -- and we blow up immediately if the fromJust should fail
            -- (which would be an internal error).
            evaluate (r `seqValue` Just r)

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
    Stale ver v -> rnf ver `seq` v `seq` b
    Failed -> b

-- | Open a 'IdeState', should be shut using 'shakeShut'.
shakeOpen :: IO LSP.LspId
          -> (LSP.FromServerMessage -> IO ()) -- ^ diagnostic handler
          -> WithProgressFunc
          -> WithIndefiniteProgressFunc
          -> ClientCapabilities
          -> Logger
          -> Debouncer NormalizedUri
          -> Maybe FilePath
          -> IdeReportProgress
          -> IdeTesting
          -> ShakeOptions
          -> Rules ()
          -> IO IdeState
shakeOpen getLspId eventer withProgress withIndefiniteProgress clientCapabilities logger debouncer
  shakeProfileDir (IdeReportProgress reportProgress) ideTesting@(IdeTesting testing) opts rules = mdo

    inProgress <- newVar HMap.empty
    us <- mkSplitUniqSupply 'r'
    ideNc <- newIORef (initNameCache us knownKeyNames)
    (shakeExtras, stopProgressReporting) <- do
        globals <- newVar HMap.empty
        state <- newVar HMap.empty
        diagnostics <- newVar mempty
        hiddenDiagnostics <- newVar mempty
        publishedDiagnostics <- newVar mempty
        positionMapping <- newVar HMap.empty
        knownTargetsVar <- newVar $ hashed HMap.empty
        let restartShakeSession = shakeRestart ideState
        let session = shakeSession
        mostRecentProgressEvent <- newTVarIO KickCompleted
        let progressUpdate = atomically . writeTVar mostRecentProgressEvent
        progressAsync <- async $
            when reportProgress $
                progressThread mostRecentProgressEvent inProgress
        exportsMap <- newVar mempty

        actionQueue <- newQueue

        pure (ShakeExtras{..}, cancel progressAsync)
    (shakeDbM, shakeClose) <-
        shakeOpenDatabase
            opts { shakeExtra = addShakeExtra shakeExtras $ shakeExtra opts }
            rules
    shakeDb <- shakeDbM
    initSession <- newSession shakeExtras shakeDb []
    shakeSession <- newMVar initSession
    let ideState = IdeState{..}

    IdeOptions{ optOTMemoryProfiling = IdeOTMemoryProfiling otProfilingEnabled } <- getIdeOptionsIO shakeExtras
    startTelemetry otProfilingEnabled logger $ state shakeExtras

    return ideState
    where
        -- The progress thread is a state machine with two states:
        --   1. Idle
        --   2. Reporting a kick event
        -- And two transitions, modelled by 'ProgressEvent':
        --   1. KickCompleted - transitions from Reporting into Idle
        --   2. KickStarted - transitions from Idle into Reporting
        progressThread mostRecentProgressEvent inProgress = progressLoopIdle
          where
            progressLoopIdle = do
                atomically $ do
                    v <- readTVar mostRecentProgressEvent
                    case v of
                        KickCompleted -> STM.retry
                        KickStarted -> return ()
                asyncReporter <- async lspShakeProgress
                progressLoopReporting asyncReporter
            progressLoopReporting asyncReporter = do
                atomically $ do
                    v <- readTVar mostRecentProgressEvent
                    case v of
                        KickStarted -> STM.retry
                        KickCompleted -> return ()
                cancel asyncReporter
                progressLoopIdle

            lspShakeProgress = do
                -- first sleep a bit, so we only show progress messages if it's going to take
                -- a "noticable amount of time" (we often expect a thread kill to arrive before the sleep finishes)
                unless testing $ sleep 0.1
                lspId <- getLspId
                u <- ProgressTextToken . T.pack . show . hashUnique <$> newUnique
                eventer $ LSP.ReqWorkDoneProgressCreate $
                  LSP.fmServerWorkDoneProgressCreateRequest lspId $
                    LSP.WorkDoneProgressCreateParams { _token = u }
                bracket_ (start u) (stop u) (loop u Nothing)
                where
                    start id = eventer $ LSP.NotWorkDoneProgressBegin $
                      LSP.fmServerWorkDoneProgressBeginNotification
                        LSP.ProgressParams
                            { _token = id
                            , _value = WorkDoneProgressBeginParams
                              { _title = "Processing"
                              , _cancellable = Nothing
                              , _message = Nothing
                              , _percentage = Nothing
                              }
                            }
                    stop id = eventer $ LSP.NotWorkDoneProgressEnd $
                      LSP.fmServerWorkDoneProgressEndNotification
                        LSP.ProgressParams
                            { _token = id
                            , _value = WorkDoneProgressEndParams
                              { _message = Nothing
                              }
                            }
                    sample = 0.1
                    loop id prev = do
                        sleep sample
                        current <- readVar inProgress
                        let done = length $ filter (== 0) $ HMap.elems current
                        let todo = HMap.size current
                        let next = Just $ T.pack $ show done <> "/" <> show todo
                        when (next /= prev) $
                            eventer $ LSP.NotWorkDoneProgressReport $
                              LSP.fmServerWorkDoneProgressReportNotification
                                LSP.ProgressParams
                                    { _token = id
                                    , _value = LSP.WorkDoneProgressReportParams
                                    { _cancellable = Nothing
                                    , _message = next
                                    , _percentage = Nothing
                                    }
                                    }
                        loop id next

shakeProfile :: IdeState -> FilePath -> IO ()
shakeProfile IdeState{..} = shakeProfileDatabase shakeDb

shakeShut :: IdeState -> IO ()
shakeShut IdeState{..} = withMVar shakeSession $ \runner -> do
    -- Shake gets unhappy if you try to close when there is a running
    -- request so we first abort that.
    void $ cancelShakeSession runner
    shakeClose
    stopProgressReporting


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
              res <- shakeDatabaseProfile shakeProfileDir shakeDb
              let profile = case res of
                      Just fp -> ", profile saved at " <> fp
                      _ -> ""
              let msg = T.pack $ "Restarting build session (aborting the previous one took "
                              ++ showDuration stopTime ++ profile ++ ")"
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
    let notif = LSP.NotLogMessage $ LSP.NotificationMessage "2.0" LSP.WindowLogMessage
                                  $ LSP.LogMessageParams LSP.MtLog msg
    when isTestMode $ eventer extras notif


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
newSession :: ShakeExtras -> ShakeDatabase -> [DelayedActionInternal] -> IO ShakeSession
newSession extras@ShakeExtras{..} shakeDb acts = do
    reenqueued <- atomically $ peekInProgress actionQueue
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

        workRun restore = withSpan "Shake session" $ \otSpan -> do
          let acts' = pumpActionThread otSpan : map (run otSpan) (reenqueued ++ acts)
          res <- try @SomeException (restore $ shakeRunDatabase shakeDb acts')
          let res' = case res of
                      Left e -> "exception: " <> displayException e
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
        do newState <- modifyVar state $ \values -> do
               values <- evaluate $ HMap.filterWithKey (\(file, _) _ -> keep file) values
               return $! dupe values
           modifyVar_ diagnostics $ \diags -> return $! filterDiagnostics keep diags
           modifyVar_ hiddenDiagnostics $ \hdiags -> return $! filterDiagnostics keep hdiags
           modifyVar_ publishedDiagnostics $ \diags -> return $! HMap.filterWithKey (\uri _ -> keep (fromUri uri)) diags
           let versionsForFile =
                   HMap.fromListWith Set.union $
                   mapMaybe (\((file, _key), v) -> (filePathToUri' file,) . Set.singleton <$> valueVersion v) $
                   HMap.toList newState
           modifyVar_ positionMapping $ \mappings -> return $! filterVersionMap versionsForFile mappings

-- | Define a new Rule without early cutoff
define
    :: IdeRule k v
    => (k -> NormalizedFilePath -> Action (IdeResult v)) -> Rules ()
define op = defineEarlyCutoff $ \k v -> (Nothing,) <$> op k v

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
        Just v -> return v

newtype IdeAction a = IdeAction { runIdeActionT  :: (ReaderT ShakeExtras IO) a }
    deriving newtype (MonadReader ShakeExtras, MonadIO, Functor, Applicative, Monad)

-- | IdeActions are used when we want to return a result immediately, even if it
-- is stale Useful for UI actions like hover, completion where we don't want to
-- block.
runIdeAction :: String -> ShakeExtras -> IdeAction a -> IO a
runIdeAction _herald s i = runReaderT (runIdeActionT i) s

askShake :: IdeAction ShakeExtras
askShake = ask

mkUpdater :: MaybeT IdeAction NameCacheUpdater
mkUpdater = do
  ref <- lift $ ideNc <$> askShake
  pure $ NCU (upNameCache ref)

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
      a <- wait
      r <- getValues state key file
      case r of
        Nothing -> return $ FastResult Nothing (pure a)
        Just v -> do
          res <- lastValueIO s file v
          pure $ FastResult res (pure a)
    -- Otherwise, use the computed value even if it's out of date.
    Just v -> do
      res <- lastValueIO s file v
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
        Just v -> return v


-- | When we depend on something that reported an error, and we fail as a direct result, throw BadDependency
--   which short-circuits the rest of the action
data BadDependency = BadDependency String deriving Show
instance Exception BadDependency

isBadDependency :: SomeException -> Bool
isBadDependency x
    | Just (x :: ShakeException) <- fromException x = isBadDependency $ shakeExceptionInner x
    | Just (_ :: BadDependency) <- fromException x = True
    | otherwise = False

newtype Q k = Q (k, NormalizedFilePath)
    deriving newtype (Eq, Hashable, NFData)

instance Binary k => Binary (Q k) where
    put (Q (k, fp)) = put (k, fp)
    get = do
        (k, fp) <- get
        -- The `get` implementation of NormalizedFilePath
        -- does not handle empty file paths so we
        -- need to handle this ourselves here.
        pure (Q (k, toNormalizedFilePath' fp))

instance Show k => Show (Q k) where
    show (Q (k, file)) = show k ++ "; " ++ fromNormalizedFilePath file

-- | Invariant: the 'v' must be in normal form (fully evaluated).
--   Otherwise we keep repeatedly 'rnf'ing values taken from the Shake database
newtype A v = A (Value v)
    deriving Show

instance NFData (A v) where rnf (A v) = v `seq` ()

-- In the Shake database we only store one type of key/result pairs,
-- namely Q (question) / A (answer).
type instance RuleResult (Q k) = A (RuleResult k)


-- | Plural version of 'use'
uses :: IdeRule k v
    => k -> [NormalizedFilePath] -> Action [Maybe v]
uses key files = map (\(A value) -> currentValue value) <$> apply (map (Q . (key,)) files)

-- | Return the last computed result which might be stale.
usesWithStale :: IdeRule k v
    => k -> [NormalizedFilePath] -> Action [Maybe (v, PositionMapping)]
usesWithStale key files = do
    values <- map (\(A value) -> value) <$> apply (map (Q . (key,)) files)
    zipWithM lastValue files values

-- | Define a new Rule with early cutoff
defineEarlyCutoff
    :: IdeRule k v
    => (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, IdeResult v))
    -> Rules ()
defineEarlyCutoff op = addBuiltinRule noLint noIdentity $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode -> otTracedAction key file isSuccess $ do
    extras@ShakeExtras{state, inProgress} <- getShakeExtras
    -- don't do progress for GetFileExists, as there are lots of non-nodes for just that one key
    (if show key == "GetFileExists" then id else withProgressVar inProgress file) $ do
        val <- case old of
            Just old | mode == RunDependenciesSame -> do
                v <- liftIO $ getValues state key file
                case v of
                    -- No changes in the dependencies and we have
                    -- an existing result.
                    Just v -> return $ Just $ RunResult ChangedNothing old $ A v
                    _ -> return Nothing
            _ -> return Nothing
        case val of
            Just res -> return res
            Nothing -> do
                (bs, (diags, res)) <- actionCatch
                    (do v <- op key file; liftIO $ evaluate $ force v) $
                    \(e :: SomeException) -> pure (Nothing, ([ideErrorText file $ T.pack $ show e | not $ isBadDependency e],Nothing))
                modTime <- liftIO $ (currentValue =<<) <$> getValues state GetModificationTime file
                (bs, res) <- case res of
                    Nothing -> do
                        staleV <- liftIO $ getValues state key file
                        pure $ case staleV of
                            Nothing -> (toShakeValue ShakeResult bs, Failed)
                            Just v -> case v of
                                Succeeded ver v -> (toShakeValue ShakeStale bs, Stale ver v)
                                Stale ver v -> (toShakeValue ShakeStale bs, Stale ver v)
                                Failed -> (toShakeValue ShakeResult bs, Failed)
                    Just v -> pure (maybe ShakeNoCutoff ShakeResult bs, Succeeded (vfsVersion =<< modTime) v)
                liftIO $ setValues state key file res
                updateFileDiagnostics file (Key key) extras $ map (\(_,y,z) -> (y,z)) diags
                let eq = case (bs, fmap decodeShakeValue old) of
                        (ShakeResult a, Just (ShakeResult b)) -> a == b
                        (ShakeStale a, Just (ShakeStale b)) -> a == b
                        -- If we do not have a previous result
                        -- or we got ShakeNoCutoff we always return False.
                        _ -> False
                return $ RunResult
                    (if eq then ChangedRecomputeSame else ChangedRecomputeDiff)
                    (encodeShakeValue bs) $
                    A res
  where
    withProgressVar :: (Eq a, Hashable a) => Var (HMap.HashMap a Int) -> a -> Action b -> Action b
    withProgressVar var file = actionBracket (f succ) (const $ f pred) . const
        -- This functions are deliberately eta-expanded to avoid space leaks.
        -- Do not remove the eta-expansion without profiling a session with at
        -- least 1000 modifications.
        where f shift = modifyVar_ var $ \x -> evaluate $ HMap.insertWith (\_ x -> shift x) file (shift 0) x

isSuccess :: RunResult (A v) -> Bool
isSuccess (RunResult _ _ (A Failed)) = False
isSuccess _ = True

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
defineOnDisk act = addBuiltinRule noLint noIdentity $
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

toShakeValue :: (BS.ByteString -> ShakeValue) -> Maybe BS.ByteString -> ShakeValue
toShakeValue = maybe ShakeNoCutoff

data ShakeValue
    = ShakeNoCutoff
    -- ^ This is what we use when we get Nothing from
    -- a rule.
    | ShakeResult !BS.ByteString
    -- ^ This is used both for `Failed`
    -- as well as `Succeeded`.
    | ShakeStale !BS.ByteString
    deriving (Generic, Show)

instance NFData ShakeValue

encodeShakeValue :: ShakeValue -> BS.ByteString
encodeShakeValue = \case
    ShakeNoCutoff -> BS.empty
    ShakeResult r -> BS.cons 'r' r
    ShakeStale r -> BS.cons 's' r

decodeShakeValue :: BS.ByteString -> ShakeValue
decodeShakeValue bs = case BS.uncons bs of
    Nothing -> ShakeNoCutoff
    Just (x, xs)
      | x == 'r' -> ShakeResult xs
      | x == 's' -> ShakeStale xs
      | otherwise -> error $ "Failed to parse shake value " <> show bs


updateFileDiagnostics :: MonadIO m
  => NormalizedFilePath
  -> Key
  -> ShakeExtras
  -> [(ShowDiagnostic,Diagnostic)] -- ^ current results
  -> m ()
updateFileDiagnostics fp k ShakeExtras{diagnostics, hiddenDiagnostics, publishedDiagnostics, state, debouncer, eventer} current = liftIO $ do
    modTime <- (currentValue =<<) <$> getValues state GetModificationTime fp
    let (currentShown, currentHidden) = partition ((== ShowDiag) . fst) current
        uri = filePathToUri' fp
        ver = vfsVersion =<< modTime
        updateDiagnosticsWithForcing new store = do
            store' <- evaluate $ setStageDiagnostics uri ver (T.pack $ show k) new store
            new' <- evaluate $ getUriDiagnostics uri store'
            return (store', new')
    mask_ $ do
        -- Mask async exceptions to ensure that updated diagnostics are always
        -- published. Otherwise, we might never publish certain diagnostics if
        -- an exception strikes between modifyVar but before
        -- publishDiagnosticsNotification.
        newDiags <- modifyVar diagnostics $ updateDiagnosticsWithForcing $ map snd currentShown
        _ <- modifyVar hiddenDiagnostics $  updateDiagnosticsWithForcing $ map snd currentHidden
        let uri = filePathToUri' fp
        let delay = if null newDiags then 0.1 else 0
        registerEvent debouncer delay uri $ do
             mask_ $ modifyVar_ publishedDiagnostics $ \published -> do
                 let lastPublish = HMap.lookupDefault [] uri published
                 when (lastPublish /= newDiags) $
                     eventer $ publishDiagnosticsNotification (fromNormalizedUri uri) newDiags
                 pure $! HMap.insert uri newDiags published

publishDiagnosticsNotification :: Uri -> [Diagnostic] -> LSP.FromServerMessage
publishDiagnosticsNotification uri diags =
    LSP.NotPublishDiagnostics $
    LSP.NotificationMessage "2.0" LSP.TextDocumentPublishDiagnostics $
    LSP.PublishDiagnosticsParams uri (List diags)

newtype Priority = Priority Double

setPriority :: Priority -> Action ()
setPriority (Priority p) = reschedule p

sendEvent :: LSP.FromServerMessage -> Action ()
sendEvent e = do
    ShakeExtras{eventer} <- getShakeExtras
    liftIO $ eventer e

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
setStageDiagnostics uri ver stage diags ds = newDiagsStore where
    -- When 'ver' is a new version, updateDiagnostics throws away diagnostics from all stages
    -- This interacts bady with early cutoff, so we make sure to preserve diagnostics
    -- from other stages when calling updateDiagnostics
    -- But this means that updateDiagnostics cannot be called concurrently
    -- for different stages anymore
    updatedDiags = Map.insert (Just stage) (SL.toSortedList diags) oldDiags
    oldDiags = case HMap.lookup uri ds of
            Just (StoreItem _ byStage) -> byStage
            _ -> Map.empty
    newDiagsStore = updateDiagnostics ds uri ver updatedDiags


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
        pure $! HMap.insert uri updatedMapping allMappings
  where
    shared_change = mkDelta changes
