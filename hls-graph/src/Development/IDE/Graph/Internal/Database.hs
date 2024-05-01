-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Development.IDE.Graph.Internal.Database (newDatabase, incDatabase, build, getDirtySet, getKeysAndVisitAge) where

import           Prelude                              hiding (unzip)

import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import           Control.Concurrent.STM.Stats         (STM, atomically,
                                                       atomicallyNamed,
                                                       modifyTVar', newTVarIO,
                                                       readTVarIO)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict     as State
import           Data.Dynamic
import           Data.Either
import           Data.Foldable                        (for_, traverse_)
import           Data.IORef.Extra
import           Data.List.NonEmpty                   (unzip)
import           Data.Maybe
import           Data.Traversable                     (for)
import           Data.Tuple.Extra
import           Debug.Trace                          (traceM)
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import qualified Focus
import qualified ListT
import qualified StmContainers.Map                    as SMap
import           System.IO.Unsafe
import           System.Time.Extra                    (duration, sleep)


newDatabase :: Dynamic -> TheRules -> IO Database
newDatabase databaseExtra databaseRules = do
    databaseStep <- newTVarIO $ Step 0
    databaseValues <- atomically SMap.new
    pure Database{..}

-- | Increment the step and mark dirty.
--   Assumes that the database is not running a build
incDatabase :: Database -> Maybe [Key] -> IO ()
-- only some keys are dirty
incDatabase db (Just kk) = do
    atomicallyNamed "incDatabase" $ modifyTVar'  (databaseStep db) $ \(Step i) -> Step $ i + 1
    transitiveDirtyKeys <- transitiveDirtySet db kk
    for_ (toListKeySet transitiveDirtyKeys) $ \k ->
        -- Updating all the keys atomically is not necessary
        -- since we assume that no build is mutating the db.
        -- Therefore run one transaction per key to minimise contention.
        atomicallyNamed "incDatabase" $ SMap.focus updateDirty k (databaseValues db)

-- all keys are dirty
incDatabase db Nothing = do
    atomically $ modifyTVar'  (databaseStep db) $ \(Step i) -> Step $ i + 1
    let list = SMap.listT (databaseValues db)
    atomicallyNamed "incDatabase - all " $ flip ListT.traverse_ list $ \(k,_) ->
        SMap.focus updateDirty k (databaseValues db)

updateDirty :: Monad m => Focus.Focus KeyDetails m ()
updateDirty = Focus.adjust $ \(KeyDetails status rdeps) ->
            let status'
                  | Running _ _ _ x <- status = Dirty x
                  | Clean x <- status = Dirty (Just x)
                  | otherwise = status
            in KeyDetails status' rdeps
-- | Unwrap and build a list of keys in parallel
build
    :: forall f key value . (Traversable f, RuleResult key ~ value, Typeable key, Show key, Hashable key, Eq key, Typeable value)
    => Database -> Stack -> f key -> IO (f Key, f value)
-- build _ st k | traceShow ("build", st, k) False = undefined
build db stack keys = do
    built <- runAIO $ do
        built <- builder db stack (fmap newKey keys)
        case built of
          Left clean  -> return clean
          Right dirty -> liftIO dirty
    let (ids, vs) = unzip built
    pure (ids, fmap (asV . resultValue) vs)
    where
        asV :: Value -> value
        asV (Value x) = unwrapDynamic x

-- | Build a list of keys and return their results.
--  If none of the keys are dirty, we can return the results immediately.
--  Otherwise, a blocking computation is returned *which must be evaluated asynchronously* to avoid deadlock.
builder
    :: Traversable f => Database -> Stack -> f Key -> AIO (Either (f (Key, Result)) (IO (f (Key, Result))))
-- builder _ st kk | traceShow ("builder", st,kk) False = undefined
builder db@Database{..} stack keys = withRunInIO $ \(RunInIO run) -> do
    -- Things that I need to force before my results are ready
    toForce <- liftIO $ newTVarIO []
    current <- liftIO $ readTVarIO databaseStep
    results <- liftIO $ for keys $ \id ->
        -- Updating the status of all the dependencies atomically is not necessary.
        -- Therefore, run one transaction per dep. to avoid contention
        atomicallyNamed "builder" $ do
            -- Spawn the id if needed
            status <- SMap.lookup id databaseValues
            val <- case viewDirty current $ maybe (Dirty Nothing) keyStatus status of
                Clean r -> pure r
                Running _ force val _
                  | memberStack id stack -> throw $ StackException stack
                  | otherwise -> do
                    modifyTVar' toForce (Wait force :)
                    pure val
                Dirty s -> do
                    let act = run (refresh db stack id s)
                        (force, val) = splitIO (join act)
                    SMap.focus (updateStatus $ Running current force val s) id databaseValues
                    modifyTVar' toForce (Spawn force:)
                    pure val

            pure (id, val)

    toForceList <- liftIO $ readTVarIO toForce
    let waitAll = run $ waitConcurrently_ toForceList
    case toForceList of
        [] -> return $ Left results
        _ -> return $ Right $ do
                waitAll
                pure results

isDirty :: Foldable t => Result -> t (a, Result) -> Bool
isDirty me = any (\(_,dep) -> resultBuilt me < resultChanged dep)

-- | Refresh dependencies for a key and compute the key:
-- The refresh the deps linearly(last computed order of the deps for the key).
-- If any of the deps is dirty in the process, we jump to the actual computation of the key
-- and shortcut the refreshing of the rest of the deps.
-- * If no dirty dependencies and we have evaluated the key previously, then we refresh it in the current thread.
--   This assumes that the implementation will be a lookup
-- * Otherwise, we spawn a new thread to refresh the dirty deps (if any) and the key itself
refreshDeps :: KeySet -> Database -> Stack -> Key -> Result -> [KeySet] -> AIO Result
refreshDeps visited db stack key result = \case
    -- no more deps to refresh
    [] -> liftIO $ compute db stack key RunDependenciesSame (Just result)
    (dep:deps) -> do
        let newVisited = dep <> visited
        res <- builder db stack (toListKeySet (dep `differenceKeySet` visited))
        case res of
            Left res ->  if isDirty result res
                -- restart the computation if any of the deps are dirty
                then liftIO $ compute db stack key RunDependenciesChanged (Just result)
                -- else kick the rest of the deps
                else refreshDeps newVisited db stack key result deps
            Right iores -> do
                res <- liftIO iores
                if isDirty result res
                    then liftIO $ compute db stack key RunDependenciesChanged (Just result)
                    else refreshDeps newVisited db stack key result deps

-- | Refresh a key:
refresh :: Database -> Stack -> Key -> Maybe Result -> AIO (IO Result)
-- refresh _ st k _ | traceShow ("refresh", st, k) False = undefined
refresh db stack key result = case (addStack key stack, result) of
    (Left e, _) -> throw e
    (Right stack, Just me@Result{resultDeps = ResultDeps deps}) -> asyncWithCleanUp $ refreshDeps mempty db stack key me (reverse deps)
    (Right stack, _) ->
        asyncWithCleanUp $ liftIO $ compute db stack key RunDependenciesChanged result

-- | Compute a key.
compute :: Database -> Stack -> Key -> RunMode -> Maybe Result -> IO Result
-- compute _ st k _ _ | traceShow ("compute", st, k) False = undefined
compute db@Database{..} stack key mode result = do
    let act = runRule databaseRules key (fmap resultData result) mode
    deps <- newIORef UnknownDeps
    (execution, RunResult{..}) <-
        duration $ runReaderT (fromAction act) $ SAction db deps stack
    built <- readTVarIO databaseStep
    deps <- readIORef deps
    let changed = if runChanged == ChangedRecomputeDiff then built else maybe built resultChanged result
        built' = if runChanged /= ChangedNothing then built else changed
        -- only update the deps when the rule ran with changes
        actualDeps = if runChanged /= ChangedNothing then deps else previousDeps
        previousDeps= maybe UnknownDeps resultDeps result
    let res = Result runValue built' changed built actualDeps execution runStore
    case getResultDepsDefault mempty actualDeps of
        deps | not (nullKeySet deps)
            && runChanged /= ChangedNothing
                    -> do
            -- IMPORTANT: record the reverse deps **before** marking the key Clean.
            -- If an async exception strikes before the deps have been recorded,
            -- we won't be able to accurately propagate dirtiness for this key
            -- on the next build.
            void $
                updateReverseDeps key db
                    (getResultDepsDefault mempty previousDeps)
                    deps
        _ -> pure ()
    atomicallyNamed "compute and run hook" $ do
        runHook
        SMap.focus (updateStatus $ Clean res) key databaseValues
    pure res

updateStatus :: Monad m => Status -> Focus.Focus KeyDetails m ()
updateStatus res = Focus.alter
    (Just . maybe (KeyDetails res mempty)
    (\it -> it{keyStatus = res}))

-- | Returns the set of dirty keys annotated with their age (in # of builds)
getDirtySet :: Database -> IO [(Key, Int)]
getDirtySet db = do
    Step curr <- readTVarIO (databaseStep db)
    dbContents <- getDatabaseValues db
    let calcAge Result{resultBuilt = Step x} = curr - x
        calcAgeStatus (Dirty x)=calcAge <$> x
        calcAgeStatus _         = Nothing
    return $ mapMaybe (secondM calcAgeStatus) dbContents

-- | Returns an approximation of the database keys,
--   annotated with how long ago (in # builds) they were visited
getKeysAndVisitAge :: Database -> IO [(Key, Int)]
getKeysAndVisitAge db = do
    values <- getDatabaseValues db
    Step curr <- readTVarIO (databaseStep db)
    let keysWithVisitAge = mapMaybe (secondM (fmap getAge . getResult)) values
        getAge Result{resultVisited = Step s} = curr - s
    return keysWithVisitAge
--------------------------------------------------------------------------------
-- Lazy IO trick

data Box a = Box {fromBox :: a}

-- | Split an IO computation into an unsafe lazy value and a forcing computation
splitIO :: IO a -> (IO (), a)
splitIO act = do
    let act2 = Box <$> act
    let res = unsafePerformIO act2
    (void $ evaluate res, fromBox res)

--------------------------------------------------------------------------------
-- Reverse dependencies

-- | Update the reverse dependencies of an Id
updateReverseDeps
    :: Key        -- ^ Id
    -> Database
    -> KeySet -- ^ Previous direct dependencies of Id
    -> KeySet -- ^ Current direct dependencies of Id
    -> IO ()
-- mask to ensure that all the reverse dependencies are updated
updateReverseDeps myId db prev new = do
    forM_ (toListKeySet $ prev `differenceKeySet` new) $ \d ->
         doOne (deleteKeySet myId) d
    forM_ (toListKeySet new) $
        doOne (insertKeySet myId)
    where
        alterRDeps f =
            Focus.adjust (onKeyReverseDeps f)
        -- updating all the reverse deps atomically is not needed.
        -- Therefore, run individual transactions for each update
        -- in order to avoid contention
        doOne f id = atomicallyNamed "updateReverseDeps" $
            SMap.focus (alterRDeps f) id (databaseValues db)

getReverseDependencies :: Database -> Key -> STM (Maybe KeySet)
getReverseDependencies db = (fmap.fmap) keyReverseDeps  . flip SMap.lookup (databaseValues db)

transitiveDirtySet :: Foldable t => Database -> t Key -> IO KeySet
transitiveDirtySet database = flip State.execStateT mempty . traverse_ loop
  where
    loop x = do
        seen <- State.get
        if x `memberKeySet` seen then pure () else do
            State.put (insertKeySet x seen)
            next <- lift $ atomically $ getReverseDependencies database x
            traverse_ loop (maybe mempty toListKeySet next)

--------------------------------------------------------------------------------
-- Asynchronous computations with cancellation

-- | A simple monad to implement cancellation on top of 'Async',
--   generalizing 'withAsync' to monadic scopes.
newtype AIO a = AIO { unAIO :: ReaderT (IORef [Async ()]) IO a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

-- | Run the monadic computation, cancelling all the spawned asyncs if an exception arises
runAIO :: AIO a -> IO a
runAIO (AIO act) = do
    asyncs <- newIORef []
    runReaderT act asyncs `onException` cleanupAsync asyncs

-- | Like 'async' but with built-in cancellation.
--   Returns an IO action to wait on the result.
asyncWithCleanUp :: AIO a -> AIO (IO a)
asyncWithCleanUp act = do
    st <- AIO ask
    io <- unliftAIO act
    -- mask to make sure we keep track of the spawned async
    liftIO $ uninterruptibleMask $ \restore -> do
        a <- async $ restore io
        atomicModifyIORef'_ st (void a :)
        return $ wait a

unliftAIO :: AIO a -> AIO (IO a)
unliftAIO act = do
    st <- AIO ask
    return $ runReaderT (unAIO act) st

newtype RunInIO = RunInIO (forall a. AIO a -> IO a)

withRunInIO :: (RunInIO -> AIO b) -> AIO b
withRunInIO k = do
    st <- AIO ask
    k $ RunInIO (\aio -> runReaderT (unAIO aio) st)

cleanupAsync :: IORef [Async a] -> IO ()
-- mask to make sure we interrupt all the asyncs
cleanupAsync ref = uninterruptibleMask $ \unmask -> do
    asyncs <- atomicModifyIORef' ref ([],)
    -- interrupt all the asyncs without waiting
    mapM_ (\a -> throwTo (asyncThreadId a) AsyncCancelled) asyncs
    -- Wait until all the asyncs are done
    -- But if it takes more than 10 seconds, log to stderr
    unless (null asyncs) $ do
        let warnIfTakingTooLong = unmask $ forever $ do
                sleep 10
                traceM "cleanupAsync: waiting for asyncs to finish"
        withAsync warnIfTakingTooLong $ \_ ->
            mapM_ waitCatch asyncs

data Wait
    = Wait {justWait :: !(IO ())}
    | Spawn {justWait :: !(IO ())}

fmapWait :: (IO () -> IO ()) -> Wait -> Wait
fmapWait f (Wait io)  = Wait (f io)
fmapWait f (Spawn io) = Spawn (f io)

waitOrSpawn :: Wait -> IO (Either (IO ()) (Async ()))
waitOrSpawn (Wait io)  = pure $ Left io
waitOrSpawn (Spawn io) = Right <$> async io

waitConcurrently_ :: [Wait] -> AIO ()
waitConcurrently_ [] = pure ()
waitConcurrently_ [one] = liftIO $ justWait one
waitConcurrently_ many = do
    ref <- AIO ask
    -- spawn the async computations.
    -- mask to make sure we keep track of all the asyncs.
    (asyncs, syncs) <- liftIO $ uninterruptibleMask $ \unmask -> do
        waits <- liftIO $ traverse (waitOrSpawn . fmapWait unmask) many
        let (syncs, asyncs) = partitionEithers waits
        liftIO $ atomicModifyIORef'_ ref (asyncs ++)
        return (asyncs, syncs)
    -- work on the sync computations
    liftIO $ sequence_ syncs
    -- wait for the async computations before returning
    liftIO $ traverse_ wait asyncs
