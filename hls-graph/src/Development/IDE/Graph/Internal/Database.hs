-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Development.IDE.Graph.Internal.Database (compute, newDatabase, incDatabase, build, getDirtySet, getKeysAndVisitAge, AsyncParentKill(..), computeToPreserve, getRunTimeRDeps, spawnAsyncWithDbRegistration) where

import           Prelude                              hiding (unzip)

import           Control.Concurrent.STM.Stats         (STM, atomicallyNamed,
                                                       modifyTVar', newTVarIO,
                                                       readTVar, readTVarIO,
                                                       retry)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Reader
import           Data.Dynamic
import           Data.Foldable                        (foldrM)
import           Data.IORef.Extra
import           Data.Maybe
import           Data.Traversable                     (for)
import           Data.Tuple.Extra
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Internal.Types ()
import qualified Focus
import qualified ListT
import qualified StmContainers.Map                    as SMap
import           System.Time.Extra                    (duration)
import           UnliftIO                             (MVar, atomically,
                                                       newEmptyMVar, putMVar,
                                                       takeMVar)

import qualified Data.List                            as List
import qualified UnliftIO.Exception                   as UE

#if MIN_VERSION_base(4,19,0)
import           Data.Functor                         (unzip)
#else
import           Data.List.NonEmpty                   (unzip)
#endif


newDatabase :: (String -> IO ()) -> ActionQueue -> Dynamic -> TheRules -> IO Database
newDatabase dataBaseLogger databaseActionQueue databaseExtra databaseRules = do
    databaseStep <- newTVarIO $ Step 0
    databaseThreads <- newTVarIO []
    databaseValuesLock <- newTVarIO True
    databaseValues <- atomically SMap.new
    databaseRRuntimeDep <- atomically SMap.new
    databaseRuntimeDepRoot <- atomically SMap.new
    databaseRRuntimeDepRoot <- atomically SMap.new
    databaseTransitiveRRuntimeDepCache <- atomically SMap.new
    pure Database{..}

-- | Increment the step and mark dirty.
--   Assumes that the database is not running a build
-- only some keys are dirty
incDatabase :: Database -> Maybe (([Key], [Key]), KeySet) -> IO KeySet
incDatabase db (Just ((_oldKeys, newKeys), preserves)) = do
    atomicallyNamed "incDatabase" $ modifyTVar' (databaseStep db) $ \(Step i) -> Step $ i + 1
    forM_ newKeys $ \newKey -> atomically $ SMap.focus updateDirty newKey (databaseValues db)
    -- only upsweep the keys that are not preserved
    -- atomically $ writeUpsweepQueue (filter  (`notMemberKeySet` preserves) oldkeys ++ newKeys) db
    return $ preserves

-- all keys are dirty
incDatabase db Nothing = do
    atomically $ modifyTVar'  (databaseStep db) $ \(Step i) -> Step $ i + 1
    let list = SMap.listT (databaseValues db)
    -- all running keys are also dirty
    atomicallyNamed "incDatabase - all " $ flip ListT.traverse_ list $ \(k,_) ->
        SMap.focus updateDirty k (databaseValues db)
    return $ mempty

computeToPreserve :: Database -> KeySet -> STM (KeySet, ([Key], [Key]), Int, [Key])
computeToPreserve db dirtySet = do
  (oldKeys, newKeys, affected) <- transitiveDirtyListBottomUpDiff db (toListKeySet dirtySet) []
  pure (affected, (oldKeys, newKeys), length newKeys, [])

updateDirty :: Monad m => Focus.Focus KeyDetails m ()
updateDirty = Focus.adjust $ \(KeyDetails status rdeps) ->
            let status'
                  | Running _ x <- status = Dirty x
                  | Clean x <- status = Dirty (Just x)
                  | otherwise = status
            in KeyDetails status' rdeps


-- updateClean :: Monad m => Focus.Focus KeyDetails m ()
-- updateClean = Focus.adjust $ \(KeyDetails _ rdeps) ->
-- | Unwrap and build a list of keys in parallel
build ::
  forall f key value.
  (Traversable f, RuleResult key ~ value, Typeable key, Show key, Hashable key, Eq key, Typeable value) =>
  Key -> Database -> Stack -> f key -> IO (f Key, f value)
build pk db stack keys = do
  built <- builder pk db stack (fmap newKey keys)
  let (ids, vs) = unzip built
  pure (ids, fmap (asV . resultValue) vs)
  where
    asV :: Value -> value
    asV (Value x) = unwrapDynamic x


-- | Build a list of keys and return their results.
--  If none of the keys are dirty, we can return the results immediately.
--  Otherwise, a blocking computation is returned *which must be evaluated asynchronously* to avoid deadlock.
builder :: (Traversable f) => Key -> Database -> Stack -> f Key -> IO (f (Key, Result))
builder pk db stack keys = do
    waits <- for keys (\k -> builderOne pk db stack k)
    for waits (interpreBuildContinue db pk)

-- the first run should not block
data BuildContinue
    = BCContinue !(Maybe (MVar (Either SomeException (Key, Result))))
    | BCStop Key Result

-- interpreBuildContinue :: BuildContinue -> IO (Key, Result)
interpreBuildContinue :: Database -> Key -> (Key, BuildContinue) -> IO (Key, Result)
interpreBuildContinue _db _pk (_kid, BCStop k v) = return (k, v)
interpreBuildContinue db _pk (kid, BCContinue Nothing) = builderOneFinal db emptyStack kid
interpreBuildContinue _db _pk (_kid, BCContinue (Just barrier)) =
    takeMVar barrier >>= either throwIO pure


builderOne :: Key -> Database -> Stack -> Key -> IO (Key, BuildContinue)
builderOne parentKey db stack kid = do
    r <- builderOne' parentKey db stack kid
    return (kid, r)

builderOneFinal :: Database -> Stack -> Key -> IO (Key, Result)
builderOneFinal Database {..} stack key = do
  -- join is used to register the async
  atomicallyNamed "builder" $ do
    status <- SMap.lookup key databaseValues
    case (viewToRun $ keyStatus <$> status) of
      (Dirty _prev) -> retry
      (Clean r) -> return (key, r)
      (Running _step _s)
        | memberStack key stack -> throw $ StackException stack
        | otherwise -> retry

builderOne' :: Key -> Database -> Stack -> Key -> IO BuildContinue
builderOne' parentKey db@Database {..} stack key = UE.uninterruptibleMask $ \restore -> do
  atomicallyNamed "builder" $ insertdatabaseRuntimeDep key parentKey db
  barrier <- newEmptyMVar
  -- join is used to register the async
  join $ restore $ mask_ $ atomicallyNamed "builder" $ do
    dbNotLocked db
    status <- SMap.lookup key databaseValues
    current <- readTVar databaseStep

    case (viewToRun $ keyStatus <$> status) of
      (Dirty prev) -> do
        SMap.focus (updateStatus $ Running current prev) key databaseValues
        let register = spawnRefresh db stack key barrier prev refresh
                        -- why it is important to use rollback here

                        {- Note [Rollback is required if killed before registration]
                        It is important to use rollback here because a key might be killed before it is registered, even though it is not one of the dirty keys.
                        In this case, it would skip being marked as dirty. Therefore, we have to roll back here if it is killed, to ensure consistency.
                        -}
                        (\_ -> atomicallyNamed "builderOne rollback" $ SMap.focus updateDirty key databaseValues)
                        restore
        return $ register >> return (BCContinue (Just barrier))
      (Clean r) -> pure . pure $ BCStop key r
      (Running _step _s)
        | memberStack key stack -> throw $ StackException stack
        | otherwise -> pure . pure $ BCContinue Nothing

-- Original spawnRefresh implementation moved below to use the abstraction
-- handleResult :: (Show a1, MonadIO m) => a1 -> MVar (Either a2 (a1, b)) -> Either a2 b -> m ()
handleResult :: MonadIO m => Key -> MVar (Either SomeException (Key, b)) -> Either SomeException b -> m ()
handleResult k barrier eResult = do
    case eResult of
        Right r -> putMVar barrier (Right (k, r))
        -- accumulate the async kill info for debugging
        Left e | Just (AsyncParentKill tid s ks) <- fromException e  -> putMVar barrier (Left (toException $ AsyncParentKill tid s (k:ks)))
        Left e  -> putMVar barrier (Left e)


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
        res <- builder key db stack (toListKeySet (dep `differenceKeySet` visited))
        if isDirty result res
                -- restart the computation if any of the deps are dirty
                then compute db stack key RunDependenciesChanged (Just result)
                -- else kick the rest of the deps
                else refreshDeps newVisited db stack key result deps


refresh :: Database -> Stack -> Key -> Maybe Result -> IO Result
refresh db stack key result = case (addStack key stack, result) of
    (Left e, _) -> throw e
    (Right stack, Just me@Result{resultDeps = ResultDeps deps}) -> refreshDeps mempty db stack key me (reverse deps)
    (Right stack, _) -> compute db stack key RunDependenciesChanged result
-- | Compute a key.
compute :: Database -> Stack -> Key -> RunMode -> Maybe Result -> IO Result
compute db@Database{..} stack key mode result = do
    let act = runRule databaseRules key (fmap resultData result) mode
    deps <- liftIO $ newIORef UnknownDeps
    curStep <- liftIO $ readTVarIO databaseStep
    -- dataBaseLogger $ "Computing key: " ++ show key ++ " at step " ++ show curStep
    (execution, RunResult{..}) <-
        liftIO $ duration $ runReaderT (fromAction act) $ SAction key db deps stack
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
    let res = Result { resultValue = runValue, resultBuilt = built, resultChanged = changed, resultVisited = curStep, resultDeps = actualDeps, resultExecution = execution, resultData = runStore }
    liftIO $ atomicallyNamed "compute and run hook" $ do
        dbNotLocked db
        case getResultDepsDefault mempty actualDeps of
            deps | not (nullKeySet deps)
                && runChanged /= ChangedNothing
                        -> do
                -- IMPORTANT: record the reverse deps **before** marking the key Clean.
                -- If an async exception strikes before the deps have been recorded,
                -- we won't be able to accurately propagate dirtiness for this key
                -- on the next build.
                updateReverseDeps key db
                    (getResultDepsDefault mempty previousDeps)
                    deps
            _ -> pure ()
        runHook
        -- it might be overridden by error if another kills this thread
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

-- | Returns an approximation of the database keys, annotated with how long ago
-- they were visited in build steps.
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
    -> STM ()
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
        doOne f id = SMap.focus (alterRDeps f) id (databaseValues db)

-- compute the transitive reverse dependencies of a set of keys

-- non-root
-- inline
{-# INLINE getRunTimeRDeps #-}
getRunTimeRDeps :: Database -> Key -> STM (Maybe KeySet)
getRunTimeRDeps db k = SMap.lookup k (databaseRRuntimeDep db)

{-# INLINE getDeps #-}
getDeps :: SMap.Map Key KeySet -> Key -> STM (Maybe KeySet)
getDeps m k = SMap.lookup k m

-- Edges in the reverse-dependency graph go from a child to its parents.
-- We perform a DFS and, after exploring all outgoing edges, cons the node onto
-- the accumulator. This yields children-before-parents order directly.

-- the lefts are keys that are no longer affected, we can try to mark them clean
-- the rights are new affected keys, we need to mark them dirty
transitiveDirtyListBottomUpDiff :: Database -> [Key] -> [Key] -> STM ([Key], [Key], KeySet)
transitiveDirtyListBottomUpDiff database seeds allOldKeys = do
  (newKeys, seen) <- cacheTransitiveDirtyListBottomUpDFSWithRootKey database $ fromListKeySet seeds
  let oldKeys = filter (`notMemberKeySet` seen) allOldKeys
  return (oldKeys, newKeys, seen)

cacheTransitiveDirtyListBottomUpDFSWithRootKey :: Database -> KeySet -> STM ([Key], KeySet)
cacheTransitiveDirtyListBottomUpDFSWithRootKey db@Database{..} seeds = do
  (newKeys, seen) <- cacheTransitiveDirtyListBottomUpDFS db seeds
  -- we should put pump root keys back to seen
  -- for each new key, get its root keys and put them back to seen
  -- newKeys is for upsweep, databaseRRuntimeDepRoot only add new root keys which is not needed for upsweep
  -- but seen is for thread filtering, we need to make sure all root keys are in seen
  (_newKeys, newSeen) <- transitiveDirtyListBottomUpDFS databaseRRuntimeDepRoot seen
  let rootKey = newKey "root"
  return $ (List.delete rootKey newKeys, deleteKeySet rootKey newSeen)



cacheTransitiveDirtyListBottomUpDFS :: Database -> KeySet -> STM ([Key], KeySet)
cacheTransitiveDirtyListBottomUpDFS Database{..} seeds = do
    SMap.lookup seeds databaseTransitiveRRuntimeDepCache >>= \case
        Just v  -> return v
        Nothing -> do
            r <- transitiveDirtyListBottomUpDFS databaseRRuntimeDep seeds
            SMap.insert r seeds databaseTransitiveRRuntimeDepCache
            return r

transitiveDirtyListBottomUpDFS :: SMap.Map Key KeySet -> KeySet -> STM ([Key], KeySet)
transitiveDirtyListBottomUpDFS database seeds = do
  let go1 :: Key -> ([Key], KeySet) -> STM ([Key], KeySet)
      go1 x acc@(dirties, seen) = do
        if x `memberKeySet` seen
          then pure acc
          else do
            let newAcc = (dirties, insertKeySet x seen)
            mnext <- getDeps database x
            (newDirties, newSeen) <- foldrM go1 newAcc (maybe mempty toListKeySet mnext)
            return (x:newDirties, newSeen)
                -- if it is root key, we do not add it to the dirty list
                -- since root key is not up for upsweep
                -- but it would be in the seen list, so we would kill dirty root key async
  -- traverse all seeds
  foldrM go1 ([], mempty) (toListKeySet seeds)

-- | Original spawnRefresh using the general pattern
-- inline
{-# INLINE spawnRefresh #-}
spawnRefresh ::
  Database ->
  t ->
  Key ->
  MVar (Either SomeException (Key, Result)) ->
  Maybe Result ->
  (Database -> t -> Key -> Maybe Result -> IO Result) ->
  (SomeException -> IO ()) ->
  (forall a. IO a -> IO a) ->
  IO ()
spawnRefresh db@Database {..} stack key barrier prevResult refresher rollBack restore = do
  Step currentStep <- readTVarIO databaseStep
  spawnAsyncWithDbRegistration
    db
    (DeliverStatus currentStep ("async computation; " ++ show key) key)
    (refresher db stack key prevResult)
    (\r -> do
        case r of
            Left e  -> rollBack e
            Right _ -> return ()
        handleResult key barrier r
    ) restore

-- Attempt to clear a Dirty parent that ended up with unchanged children during this event.
-- If the parent is Dirty, and every direct child is either Clean/Exception/Running for a step < eventStep,
-- and no child changed at/after eventStep, mark parent Clean (preserving its last Clean result),
-- and recursively attempt the same for its own parents.
