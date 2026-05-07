{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}

module Development.IDE.Graph.Internal.Types where

import           Control.Concurrent.STM             (STM, TQueue, TVar, check,
                                                     flushTQueue, isEmptyTQueue,
                                                     modifyTVar', newTQueue,
                                                     newTVar, readTQueue,
                                                     readTVar, unGetTQueue,
                                                     writeTQueue)
import           Control.Exception                  (throw)
import           Control.Monad                      (forM, forM_, forever,
                                                     unless, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.RWS                  (MonadReader (local), asks)
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Bifunctor                     (second)
import qualified Data.ByteString                    as BS
import           Data.Dynamic
import           Data.Foldable                      (fold)
import qualified Data.HashMap.Strict                as Map
import           Data.HashSet                       (HashSet)
import qualified Data.HashSet                       as Set
import           Data.IORef
import           Data.List                          (intercalate, partition)
import           Data.Maybe                         (fromMaybe, isJust,
                                                     isNothing)
import           Data.Typeable
import           Debug.Trace                        (traceEventIO)
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Key
import qualified Focus
import           GHC.Conc                           ()
import           GHC.Generics                       (Generic)
import qualified ListT
import           Numeric.Natural
import           Prettyprinter
import qualified StmContainers.Map                  as SMap
import           StmContainers.Map                  (Map)
import           System.Time.Extra                  (Seconds, sleep)
import           UnliftIO                           (Async (asyncThreadId),
                                                     MonadUnliftIO,
                                                     asyncExceptionFromException,
                                                     asyncExceptionToException,
                                                     asyncWithUnmask,
                                                     atomically, cancelWith,
                                                     newEmptyTMVarIO, poll,
                                                     putTMVar, readTMVar,
                                                     readTVarIO, throwTo,
                                                     waitCatch,
                                                     withAsyncWithUnmask)
import           UnliftIO.Concurrent                (ThreadId, myThreadId)
import qualified UnliftIO.Exception                 as UE


#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative                (liftA2)
#endif

unwrapDynamic :: forall a . Typeable a => Dynamic -> a
unwrapDynamic x = fromMaybe (error msg) $ fromDynamic x
    where msg = "unwrapDynamic failed: Expected " ++ show (typeRep (Proxy :: Proxy a)) ++
                ", but got " ++ show (dynTypeRep x)

---------------------------------------------------------------------
-- RULES

type TheRules = Map.HashMap TypeRep Dynamic

-- | A computation that defines all the rules that form part of the computation graph.
--
-- 'Rules' has access to 'IO' through 'MonadIO'. Use of 'IO' is at your own risk: if
-- you write 'Rules' that throw exceptions, then you need to make sure to handle them
-- yourself when you run the resulting 'Rules'.
newtype Rules a = Rules (ReaderT SRules IO a)
    deriving newtype (Monad, Applicative, Functor, MonadIO)

data SRules = SRules {
    rulesExtra   :: !Dynamic,
    rulesActions :: !(IORef [Action ()]),
    rulesMap     :: !(IORef TheRules)
    }

---------------------------------------------------------------------
-- ACTIONS

-- | An action representing something that can be run as part of a 'Rule'.
--
-- 'Action's can be pure functions but also have access to 'IO' via 'MonadIO' and 'MonadUnliftIO.
-- It should be assumed that actions throw exceptions, these can be caught with
-- 'Development.IDE.Graph.Internal.Action.actionCatch'. In particular, it is
-- permissible to use the 'MonadFail' instance, which will lead to an 'IOException'.
newtype Action a = Action {fromAction :: ReaderT SAction IO a}
    deriving newtype (Monad, Applicative, Functor, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, MonadReader SAction)

runActionMonad :: Action a ->  SAction -> IO a
runActionMonad (Action r) s = runReaderT r s

data SAction = SAction {
    actionKey      :: !Key,
    actionDatabase :: !Database,
    actionDeps     :: !(IORef ResultDeps),
    actionStack    :: !Stack
    }

getDatabase :: Action Database
getDatabase = asks actionDatabase

getActionKey :: Action Key
getActionKey = asks actionKey

setActionKey :: Key -> Action a -> Action a
setActionKey k act = local (\s' -> s'{actionKey = k}) act


---------------------------------------------------------------------
-- DATABASE

-- | A simple priority used for annotating delayed actions.
-- Ordering is important: Debug < Info < Warning < Error
data Priority
        = Debug
        | Info
        | Warning
        | Error
        deriving (Eq, Show, Read, Ord, Enum, Bounded)

type DelayedActionInternal = DelayedAction ()
-- | A delayed action that carries an Action payload.
data DelayedAction a = DelayedAction
    { uniqueID       :: Key
    , actionName     :: String -- ^ Name we use for debugging
    , actionPriority :: Priority -- ^ Priority with which to log the action
    , getAction      :: Action a -- ^ The payload
    }
    deriving (Functor)

actionNameKey :: DelayedAction a -> String
actionNameKey d = actionName d ++ " (" ++ show (uniqueID d) ++ ")"
instance Eq (DelayedAction a) where
    a == b = uniqueID a == uniqueID b

instance Hashable (DelayedAction a) where
    hashWithSalt s = hashWithSalt s . uniqueID

instance Show (DelayedAction a) where
    show d = "DelayedAction: " ++ actionName d

-------------------------------------------------------------------------------

-- | A queue of delayed actions for the graph 'Action' monad.
data ActionQueue = ActionQueue
    { newActions :: TQueue (DelayedAction ())
    , inProgress :: TVar (HashSet (DelayedAction ()))
    }

newQueue :: IO ActionQueue
newQueue = atomically $ do
    newActions <- newTQueue
    inProgress <- newTVar mempty
    return ActionQueue {..}

pushQueue :: DelayedAction () -> ActionQueue -> STM ()
pushQueue act ActionQueue {..} = writeTQueue newActions act

-- | Append to the front of the queue
unGetQueue :: DelayedAction () -> ActionQueue -> STM ()
unGetQueue act ActionQueue {..} = unGetTQueue newActions act

-- | You must call 'doneQueue' to signal completion
popQueue :: ActionQueue -> STM (DelayedAction ())
popQueue ActionQueue {..} = do
        x <- readTQueue newActions
        modifyTVar' inProgress (Set.insert x)
        return x

popAllQueue :: ActionQueue -> STM [DelayedAction ()]
popAllQueue ActionQueue {..} = do
        xs <- flushTQueue newActions
        modifyTVar' inProgress (\s -> s `Set.union` Set.fromList xs)
        return xs

insertRunnning :: DelayedAction () -> ActionQueue ->  STM ()
insertRunnning act ActionQueue {..} = modifyTVar' inProgress (Set.insert act)

-- | Completely remove an action from the queue
abortQueue :: DelayedAction () -> ActionQueue -> STM ()
abortQueue x ActionQueue {..} = do
    qq <- flushTQueue newActions
    mapM_ (writeTQueue newActions) (filter (/= x) qq)
    modifyTVar' inProgress (Set.delete x)

-- | Mark an action as complete when called after 'popQueue'.
--   Has no effect otherwise
doneQueue :: DelayedAction () -> ActionQueue -> STM ()
doneQueue x ActionQueue {..} = do
    modifyTVar' inProgress (Set.delete x)

countQueue :: ActionQueue -> STM Natural
countQueue ActionQueue{..} = do
        backlog <- flushTQueue newActions
        mapM_ (writeTQueue newActions) backlog
        m <- Set.size <$> readTVar inProgress
        return $ fromIntegral $ length backlog + m

peekInProgress :: ActionQueue -> STM [DelayedAction ()]
peekInProgress ActionQueue {..} = Set.toList <$> readTVar inProgress

isActionQueueEmpty :: ActionQueue -> STM Bool
isActionQueueEmpty ActionQueue {..} = do
        emptyQueue <- isEmptyTQueue newActions
        inProg <- Set.null <$> readTVar inProgress
        return (emptyQueue && inProg)

data ShakeDatabase = ShakeDatabase !Int [Action ()] Database

newtype Step = Step Int
    deriving newtype (Eq,Ord,Hashable,Show,Num,Enum,Real,Integral)

data DeliverStatus = DeliverStatus
    { deliverStep :: Int
    , deliverName :: String
    , deliverKey  :: Key
    } deriving (Show)

instance Pretty DeliverStatus where
    pretty (DeliverStatus step name key) =
        pretty ("Step:" :: String) <+> pretty step <> comma
            <+> pretty ("name:" :: String) <+> pretty name <> comma
            <+> pretty ("key:" :: String) <+> pretty (show key)

getShakeStep :: MonadIO m => ShakeDatabase -> m Step
getShakeStep (ShakeDatabase _ _ db) = do
    s <- readTVarIO $ databaseStep db
    return s

lockShakeDatabaseValues :: MonadIO m => ShakeDatabase -> m ()
lockShakeDatabaseValues (ShakeDatabase _ _ db) = do
    liftIO $ atomically $ modifyTVar' (databaseValuesLock db) (const False)

unlockShakeDatabaseValues :: MonadIO m => ShakeDatabase -> m ()
unlockShakeDatabaseValues (ShakeDatabase _ _ db) = do
    liftIO $ atomically $ modifyTVar' (databaseValuesLock db) (const True)

withShakeDatabaseValuesLock :: ShakeDatabase -> IO c -> IO c
withShakeDatabaseValuesLock sdb act = do
    UE.bracket_ (lockShakeDatabaseValues sdb) (unlockShakeDatabaseValues sdb) act

dbNotLocked :: Database -> STM ()
dbNotLocked db = do
 check =<< readTVar (databaseValuesLock db)


---------------------------------------------------------------------
-- Keys
newtype Value = Value Dynamic

data KeyDetails = KeyDetails {
    keyStatus      :: !Status,
    keyReverseDeps :: !KeySet
    }

onKeyReverseDeps :: (KeySet -> KeySet) -> KeyDetails -> KeyDetails
onKeyReverseDeps f it@KeyDetails{..} =
    it{keyReverseDeps = f keyReverseDeps}

data Database = Database {
    databaseExtra                      :: Dynamic,

    databaseThreads                    :: TVar [(DeliverStatus, Async ())],

    databaseRuntimeDepRoot             :: SMap.Map Key KeySet,

    databaseRRuntimeDepRoot            :: SMap.Map Key KeySet,
    databaseRRuntimeDep                :: SMap.Map Key KeySet,
    -- it is used to compute the transitive reverse deps, so
    -- if not in any of the transitive reverse deps of a dirty node, it is clean
    -- we can skip clean the threads.
    -- this is update right before we query the database for the key result.
    databaseTransitiveRRuntimeDepCache :: SMap.Map KeySet TransitiveDirtyKeys,
    -- ^ this is a cache for transitive reverse deps if we have computed it before
    -- and the databaseRRuntimeDep did not change since last time
    -- it is very useful for large projects where many files depend on a few common files
    -- e.g. we do not want to recompute the transitive reverse deps every time we enter a letter
    -- to a file.


    dataBaseLogger                     :: String -> IO (),

    -- The action queue and
    databaseActionQueue                :: ActionQueue,


    databaseRules                      :: TheRules,
    databaseStep                       :: !(TVar Step),

    databaseValuesLock                 :: !(TVar Bool),
    -- when we restart a build, we set this to False to block any other
    -- threads from reading databaseValues
    databaseValues                     :: !(Map Key KeyDetails)

    }

data TransitiveDirtyKeys = TransitiveDirtyKeys
    { transitiveDirtyList :: ![Key]
      -- ^ Dirty keys in children-before-parents order.
    , transitiveDirtySet  :: !KeySet
      -- ^ Same transitive closure as a set, used for membership/filtering.
    } deriving Show


---------------------------------------------------------------------
-- | Remove finished asyncs from 'databaseThreads' (non-blocking).
--   Uses 'poll' to check completion without waiting.
pruneFinished :: Database -> IO ()
pruneFinished db@Database{..} = do
    threads <- readTVarIO databaseThreads
    statuses <- forM threads $ \(d,a) -> do
        p <- poll a
        return (d,a,p)
    let still = [ (d,a) | (d,a,p) <- statuses, isNothing p ]
    -- deleteDatabaseRuntimeDep of finished async keys
    forM_ statuses $ \(d,_,p) -> when (isJust p) $ do
        let k = deliverKey d
        when (k /= newKey "root") $ atomically $ deleteDatabaseRuntimeDep k db
    atomically $ modifyTVar' databaseThreads (const still)

deleteDatabaseRuntimeDep :: Key -> Database -> STM ()
deleteDatabaseRuntimeDep k db = do
    result <- SMap.lookup k (databaseRuntimeDepRoot db)
    case result of
        Nothing -> return ()
        Just deps -> do
            SMap.delete k (databaseRuntimeDepRoot db)
            -- also remove k from all its reverse deps
            forM_ (toListKeySet deps) $ \d -> do
                SMap.focus (Focus.alter (fmap (deleteKeySet k))) d (databaseRRuntimeDepRoot db)


-- record runtime reverse deps for each key,
-- if it is root key, also reverse deps so when the root key is done, we can clean up the reverse deps.
insertdatabaseRuntimeDep :: Key -> Key -> Database -> STM ()
insertdatabaseRuntimeDep k pk db = do
    if isRootKey pk || isRootKey k
        then do
            SMap.focus (Focus.alter (Just . maybe (singletonKeySet k) (insertKeySet k))) pk (databaseRuntimeDepRoot db)
            SMap.focus (Focus.alter (Just . maybe (singletonKeySet pk) (insertKeySet pk))) k (databaseRRuntimeDepRoot db)
        else do
            -- databaseRRuntimeDep only incremental, so no need to keep a reverse one
            -- Also I want to know if the database changed
            -- if changed we need to reset databaseTransitiveRRuntimeDepCache
            SMap.lookup k (databaseRRuntimeDep db) >>= \case
                Nothing -> do
                    SMap.insert (singletonKeySet pk) k (databaseRRuntimeDep db)
                    SMap.reset (databaseTransitiveRRuntimeDepCache db)
                Just s -> when (pk `notMemberKeySet` s) $ do
                    SMap.insert (insertKeySet pk s) k (databaseRRuntimeDep db)
                    SMap.reset (databaseTransitiveRRuntimeDepCache db)

-- inline
{-# INLINE isRootKey #-}
isRootKey :: Key -> Bool
isRootKey (DirectKey _a) = True
isRootKey _              = False

---------------------------------------------------------------------

-- | Abstract pattern for spawning async computations with database registration.
-- This pattern is used by spawnRefresh and can be used by other functions that need:
-- 1. Protected async creation with uninterruptibleMask
-- 2. Database thread tracking and state updates
-- 3. Controlled start coordination via barriers
-- 4. Exception safety with rollback on registration failure
-- @ inline
{-# INLINE spawnAsyncWithDbRegistration #-}
spawnAsyncWithDbRegistration :: Database -> DeliverStatus -> IO a1 -> (Either SomeException a1 -> IO ()) -> (forall a. IO a -> IO a) -> IO ()
spawnAsyncWithDbRegistration db@Database{..} deliver asyncBody handler restore = do
    startBarrier <- newEmptyTMVarIO
    -- 1. we need to make sure the thread is registered before we actually start
    -- 2. we should not start in between the restart
    -- 3. if it is killed before we start, we need to cancel the async
    let register a = do
                    dbNotLocked db
                    modifyTVar' databaseThreads ((deliver, a):)
                    -- make sure we only start after the restart
                    putTMVar startBarrier ()
    a <- asyncWithUnmask $ \restore -> (handler =<< ((restore $ atomically (readTMVar startBarrier) >> (Right <$> asyncBody)) `catch` \e@(SomeException _) -> return (Left e)))
    (restore $ atomically $ register a)
        `catch` \e@(SomeException _) -> do
                cancelWith a e
                throw e

-- inline
{-# INLINE runInThreadStmInNewThreads #-}
runInThreadStmInNewThreads :: Database -> DeliverStatus -> IO a -> (Either SomeException a -> IO ()) -> IO ()
runInThreadStmInNewThreads db deliver act handler = uninterruptibleMask $ \restore ->
        spawnAsyncWithDbRegistration db deliver act handler restore

getDataBaseStepInt :: Database -> STM Int
getDataBaseStepInt db = do
    Step s <- readTVar $ databaseStep db
    return s

data AsyncParentKill = AsyncParentKill ThreadId Step [Key]
    deriving (Show, Eq)

instance Exception AsyncParentKill where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

shutDatabase ::KeySet -> Database -> IO ()
shutDatabase dirties db@Database{..} = uninterruptibleMask $ \_unmask -> do
    -- wait for all threads to finish
    asyncs <- readTVarIO databaseThreads
    step <- readTVarIO databaseStep
    tid <- myThreadId
    let rootKey = newKey "root"
    let (toCancel, remains) = partition (\(k, _) -> deliverKey k `memberKeySet` dirties || deliverKey k == rootKey) asyncs
    atomically $ modifyTVar' databaseThreads (const remains)
    mapM_ (\(k, a) -> throwTo (asyncThreadId a) $ AsyncParentKill tid step [deliverKey k, newKey "shutDatabase"]) toCancel
    -- Wait until all the asyncs are done
    -- But if it takes more than 10 seconds, log to stderr
    unless (null asyncs) $ do
        let warnIfTakingTooLong = forever $ do
                sleep 5
                as <- readTVarIO databaseThreads
                -- poll each async: Nothing => still running
                statuses <- forM as $ \(d,a) -> do
                    p <- poll a
                    return (d, a, p)
                let still = [ (deliverName d, show (asyncThreadId a)) | (d,a,p) <- statuses, isNothing p ]
                traceEventIO $ "cleanupAsync: waiting for asyncs to finish; total=" ++ show (length as) ++ ", stillRunning=" ++ show (length still)
                traceEventIO $ "cleanupAsync: still running (deliverName, threadId) = " ++ show still
        withAsyncWithUnmask (\restore -> restore warnIfTakingTooLong) $ \_ -> mapM_ (waitCatch . snd) toCancel
        forM_ toCancel $ \(d,_p) -> do
            let k = deliverKey d
            when (k /= newKey "root") $ atomically $ deleteDatabaseRuntimeDep k db
    pruneFinished db

peekAsyncsDelivers :: MonadIO m => Database -> m [DeliverStatus]
peekAsyncsDelivers db = do
    asyncs <- readTVarIO (databaseThreads db)
    return $ fst <$> asyncs

getDatabaseValues :: Database -> IO [(Key, Status)]
getDatabaseValues = atomically
                  . (fmap.fmap) (second keyStatus)
                  . ListT.toList
                  . SMap.listT
                  . databaseValues

data Status
    = Clean !Result
    -- dirty should say why it is dirty,
    -- it should and only should be clean,
    -- once all the event has been processed,
    -- once event is represeted by a step
    | Dirty (Maybe Result)
    | Running {
        runningStep :: !Step,
        -- runningResult :: Result,     -- LAZY
        runningPrev :: !(Maybe Result)
        -- runningWait :: !(MVar (Either SomeException (Key, Result)))
        }
instance Show Status where
    show (Clean _)      = "Clean"
    show (Dirty _)      = "Dirty"
    show (Running s _ ) = "Running step " ++ show s

viewDirty :: Step -> Status -> Status
-- viewDirty currentStep (Running s re _ _) | currentStep /= s = Dirty re
viewDirty _ other = other


viewToRun :: Maybe Status -> Status
-- viewToRun _currentStep (Dirty _) = Nothing
-- viewToRun currentStep (Running s _re _ _) | currentStep /= s = Nothing
viewToRun Nothing      = (Dirty Nothing)
viewToRun (Just other) = other

getResult :: Status -> Maybe Result
getResult (Clean re)        = Just re
getResult (Dirty m_re)      = m_re
getResult (Running _ m_re ) = m_re -- watch out: this returns the previous result


data Result = Result {
    resultValue     :: !Value,
    resultBuilt     :: !Step, -- ^ the step when it was last recomputed
    resultChanged   :: !Step, -- ^ the step when it last changed
    resultVisited   :: !Step, -- ^ the step when it was last looked up
    resultDeps      :: !ResultDeps,
    resultExecution :: !Seconds, -- ^ How long it took, last time it ran
    resultData      :: !BS.ByteString
    }

-- Notice, invariant to maintain:
-- the ![KeySet] in ResultDeps need to be stored in reverse order,
-- so that we can append to it efficiently, and we need the ordering
-- so we can do a linear dependency refreshing in refreshDeps.
data ResultDeps = UnknownDeps | AlwaysRerunDeps !KeySet | ResultDeps ![KeySet]
  deriving (Eq, Show)

getResultDepsDefault :: KeySet -> ResultDeps -> KeySet
getResultDepsDefault _ (ResultDeps ids)      = fold ids
getResultDepsDefault _ (AlwaysRerunDeps ids) = ids
getResultDepsDefault def UnknownDeps         = def

mapResultDeps :: (KeySet -> KeySet) -> ResultDeps -> ResultDeps
mapResultDeps f (ResultDeps ids)      = ResultDeps $ fmap f ids
mapResultDeps f (AlwaysRerunDeps ids) = AlwaysRerunDeps $ f ids
mapResultDeps _ UnknownDeps           = UnknownDeps

instance Semigroup ResultDeps where
    UnknownDeps <> x = x
    x <> UnknownDeps = x
    AlwaysRerunDeps ids <> x = AlwaysRerunDeps (ids <> getResultDepsDefault mempty x)
    x <> AlwaysRerunDeps ids = AlwaysRerunDeps (getResultDepsDefault mempty x <> ids)
    ResultDeps ids <> ResultDeps ids' = ResultDeps (ids <> ids')

instance Monoid ResultDeps where
    mempty = UnknownDeps

---------------------------------------------------------------------
-- Running builds

-- | What mode a rule is running in, passed as an argument to 'BuiltinRun'.
data RunMode
    = RunDependenciesSame -- ^ My dependencies have not changed.
    | RunDependenciesChanged -- ^ At least one of my dependencies from last time have changed, or I have no recorded dependencies.
      deriving (Eq,Show)

instance Monoid RunMode where
    mempty = RunDependenciesSame
instance Semigroup RunMode where
    RunDependenciesSame <> b    = b
    RunDependenciesChanged <> _ = RunDependenciesChanged

instance NFData RunMode where rnf x = x `seq` ()

-- | How the output of a rule has changed.
data RunChanged
    = ChangedNothing -- ^ Nothing has changed.
    | ChangedRecomputeSame -- ^ I recomputed the value and it was the same.
    | ChangedRecomputeDiff -- ^ I recomputed the value and it was different.
      deriving (Eq,Show,Generic)
      deriving anyclass (FromJSON, ToJSON)

instance NFData RunChanged where rnf x = x `seq` ()

-- | The result of 'BuiltinRun'.
data RunResult value = RunResult
    {runChanged :: RunChanged
        -- ^ How has the 'RunResult' changed from what happened last time.
    ,runStore   :: BS.ByteString
        -- ^ The value to store in the Shake database.
    ,runValue   :: value
        -- ^ The value to return from 'Development.Shake.Rule.apply'.
    ,runHook    :: STM ()
        -- ^ The hook to run at the end of the build in the same transaction
        -- when the key is marked as clean.
    } deriving Functor

---------------------------------------------------------------------
-- EXCEPTIONS

data GraphException = forall e. Exception e => GraphException {
    target :: String, -- ^ The key that was being built
    stack  :: [String], -- ^ The stack of keys that led to this exception
    inner  :: e -- ^ The underlying exception
}
  deriving (Exception)

instance Show GraphException where
    show GraphException{..} = unlines $
        ["GraphException: " ++ target] ++
        stack ++
        ["Inner exception: " ++ show inner]

fromGraphException :: Typeable b => SomeException -> Maybe b
fromGraphException x = do
    GraphException _ _ e <- fromException x
    cast e

---------------------------------------------------------------------
-- CALL STACK

data Stack = Stack [Key] !KeySet

instance Show Stack where
    show (Stack kk _) = "Stack: " <> intercalate " -> " (map show kk)

newtype StackException = StackException Stack
  deriving (Show)

instance Exception StackException where
    fromException = fromGraphException
    toException this@(StackException (Stack stack _)) = toException $
        GraphException (show$ last stack) (map show stack) this

addStack :: Key -> Stack -> Either StackException Stack
addStack k (Stack ks is)
    | k `memberKeySet` is = Left $ StackException stack2
    | otherwise = Right stack2
    where stack2 = Stack (k:ks) (insertKeySet k is)

memberStack :: Key -> Stack -> Bool
memberStack k (Stack _ ks) = k `memberKeySet` ks

emptyStack :: Stack
emptyStack = Stack [] mempty
---------------------------------------------------------------------
-- INSTANCES

instance Semigroup a => Semigroup (Rules a) where
    a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (Rules a) where
    mempty = pure mempty
