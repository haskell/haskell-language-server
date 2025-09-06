-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Development.IDE.Graph.Internal.Database (compute, newDatabase, incDatabase, build, getDirtySet, getKeysAndVisitAge, AsyncParentKill(..)) where

import           Prelude                              hiding (unzip)

import           Control.Concurrent.STM.Stats         (STM, atomically,
                                                       atomicallyNamed,
                                                       modifyTVar', newTVarIO,
                                                       readTVar, readTVarIO,
                                                       retry)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict     as State
import           Data.Dynamic
import           Data.Foldable                        (for_, traverse_)
import           Data.IORef.Extra
import           Data.Maybe
import           Data.Traversable                     (for)
import           Data.Tuple.Extra
import           Debug.Trace                          (traceEvent)
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import qualified Focus
import qualified ListT
import qualified StmContainers.Map                    as SMap
import           System.Time.Extra                    (duration)

#if MIN_VERSION_base(4,19,0)
import           Data.Functor                         (unzip)
#else
import           Data.List.NonEmpty                   (unzip)
#endif
import           Development.IDE.WorkerThread         (TaskQueue,
                                                       awaitRunInThreadStmInNewThread)


newDatabase :: TaskQueue (IO ()) -> Dynamic -> TheRules -> IO Database
newDatabase databaseQueue databaseExtra databaseRules = do
    databaseStep <- newTVarIO $ Step 0
    databaseThreads <- newTVarIO []
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
    -- let list = SMap.listT (databaseValues db)
    -- atomicallyNamed "incDatabase - all " $ flip ListT.traverse_ list $ \(k,_) ->
    --     SMap.focus dirtyRunningKey k (databaseValues db)

-- all keys are dirty
incDatabase db Nothing = do
    atomically $ modifyTVar'  (databaseStep db) $ \(Step i) -> Step $ i + 1
    let list = SMap.listT (databaseValues db)
    -- all running keys are also dirty
    atomicallyNamed "incDatabase - all " $ flip ListT.traverse_ list $ \(k,_) ->
        SMap.focus updateDirty k (databaseValues db)

updateDirty :: Monad m => Focus.Focus KeyDetails m ()
updateDirty = Focus.adjust $ \(KeyDetails status rdeps) ->
            let status'
                  | Running _ x <- status = Dirty x
                  | Clean x <- status = Dirty (Just x)
                  | otherwise = status
            in KeyDetails status' rdeps
-- | Unwrap and build a list of keys in parallel
build
    :: forall f key value . (Traversable f, RuleResult key ~ value, Typeable key, Show key, Hashable key, Eq key, Typeable value)
    => Database -> Stack -> f key -> IO (f Key, f value)
-- build _ st k | traceShow ("build", st, k) False = undefined
build db stack keys = do
    step <- readTVarIO $ databaseStep db
    go `catch` \e@(AsyncParentKill i s) -> do
        if s == step
            then throw e
            else throw $ AsyncParentKill i $ Step (-1)
    where
    go = do
        -- step <- readTVarIO $ databaseStep db
        -- built <- mapConcurrently (builderOne db stack) (fmap newKey keys)
        built <- builder db stack (fmap newKey keys)
        let (ids, vs) = unzip built
        pure (ids, fmap (asV . resultValue) vs)
        where
            asV :: Value -> value
            asV (Value x) = unwrapDynamic x


-- | Build a list of keys and return their results.
--  If none of the keys are dirty, we can return the results immediately.
--  Otherwise, a blocking computation is returned *which must be evaluated asynchronously* to avoid deadlock.
builder :: (Traversable f) => Database -> Stack -> f Key -> IO (f (Key, Result))
-- builder _ st kk | traceShow ("builder", st,kk) False = undefined
builder db stack keys = for keys $ \k -> builderOne db stack k

builderOne :: Database -> Stack -> Key -> IO (Key, Result)
builderOne db@Database {..} stack id = do
  traceEvent ("builderOne: " ++ show id) return ()
  res <- liftIO $ atomicallyNamed "builder" $ do
    -- Spawn the id if needed
    status <- SMap.lookup id databaseValues
    current@(Step cs) <- readTVar databaseStep
    let getStep = do
            Step current <- readTVar databaseStep
            return current

    val <- case viewDirty current $ maybe (Dirty Nothing) keyStatus status of
      Dirty s -> do
        SMap.focus (updateStatus $ Running current s) id databaseValues
        traceEvent ("Starting build of key: " ++ show id ++ ", step "++ show current)
            $ awaitRunInThreadStmInNewThread getStep cs databaseQueue databaseThreads  (refresh db stack id s)
            $ \e -> atomically $ SMap.focus (updateStatus $ Exception current e s) id databaseValues
        return Nothing
      Clean r -> return $ Just r
      -- force here might contains async exceptions from previous runs
      Running _step _s
        | memberStack id stack -> throw $ StackException stack
        | otherwise -> retry
      Exception _ e _s -> throw e
    pure val
  case res of
    Just r  -> return (id, r)
    Nothing -> builderOne db stack id

-- | isDirty
-- only dirty when it's build time is older than the changed time of one of its dependencies
isDirty :: Foldable t => Result -> t (a, Result) -> Bool
isDirty me = any (\(_,dep) -> resultBuilt me < resultChanged dep)

-- | Refresh dependencies for a key and compute the key:
-- The refresh the deps linearly(last computed order of the deps for the key).
-- If any of the deps is dirty in the process, we jump to the actual computation of the key
-- and shortcut the refreshing of the rest of the deps.
-- * If no dirty dependencies and we have evaluated the key previously, then we refresh it in the current thread.
--   This assumes that the implementation will be a lookup
-- * Otherwise, we spawn a new thread to refresh the dirty deps (if any) and the key itself
refreshDeps :: KeySet -> Database -> Stack -> Key -> Result -> [KeySet] -> IO Result
refreshDeps visited db stack key result = \case
    -- no more deps to refresh
    [] -> compute db stack key RunDependenciesSame (Just result)
    (dep:deps) -> do
        let newVisited = dep <> visited
        res <- builder db stack (toListKeySet (dep `differenceKeySet` visited))
        if isDirty result res
                -- restart the computation if any of the deps are dirty
                then compute db stack key RunDependenciesChanged (Just result)
                -- else kick the rest of the deps
                else refreshDeps newVisited db stack key result deps


-- refresh :: Database -> Stack -> Key -> Maybe Result -> IO Result
-- refresh _ st k _ | traceShow ("refresh", st, k) False = undefined
refresh :: Database -> Stack -> Key -> Maybe Result -> IO Result
refresh db stack key result = case (addStack key stack, result) of
    (Left e, _) -> throw e
    (Right stack, Just me@Result{resultDeps = ResultDeps deps}) -> refreshDeps mempty db stack key me (reverse deps)
    (Right stack, _) -> compute db stack key RunDependenciesChanged result
-- | Compute a key.
compute :: Database -> Stack -> Key -> RunMode -> Maybe Result -> IO Result
-- compute _ st k _ _ | traceShow ("compute", st, k) False = undefined
compute db@Database{..} stack key mode result = do
    let act = runRule databaseRules key (fmap resultData result) mode
    deps <- liftIO $ newIORef UnknownDeps
    (execution, RunResult{..}) <-
        liftIO $ duration $ runReaderT (fromAction act) $ SAction db deps stack
    curStep <- liftIO $ readTVarIO databaseStep
    deps <- liftIO $ readIORef deps
    let lastChanged = maybe curStep resultChanged result
    let lastBuild = maybe curStep resultBuilt result
    -- changed time is always older than or equal to build time
    let (changed, built) =  case runChanged of
            -- some thing changed
            ChangedRecomputeDiff -> (curStep, curStep)
            -- recomputed is the same
            ChangedRecomputeSame -> (lastChanged, curStep)
            -- nothing changed
            ChangedNothing       -> (lastChanged, lastBuild)
    let -- only update the deps when the rule ran with changes
        actualDeps = if runChanged /= ChangedNothing then deps else previousDeps
        previousDeps= maybe UnknownDeps resultDeps result
    let res = Result runValue built changed curStep actualDeps execution runStore
    case getResultDepsDefault mempty actualDeps of
        deps | not (nullKeySet deps)
            && runChanged /= ChangedNothing
                    -> do
            -- IMPORTANT: record the reverse deps **before** marking the key Clean.
            -- If an async exception strikes before the deps have been recorded,
            -- we won't be able to accurately propagate dirtiness for this key
            -- on the next build.
            liftIO $ void $
                updateReverseDeps key db
                    (getResultDepsDefault mempty previousDeps)
                    deps
        _ -> pure ()
    liftIO $ atomicallyNamed "compute and run hook" $ do
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



