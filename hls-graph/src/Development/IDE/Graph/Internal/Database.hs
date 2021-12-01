-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Development.IDE.Graph.Internal.Database (newDatabase, incDatabase, build, getDirtySet, getKeysAndVisitAge) where

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
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HSet
import           Data.IORef.Extra
import           Data.Maybe
import           Data.Traversable                     (for)
import           Data.Tuple.Extra
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import qualified Focus
import qualified ListT
import qualified StmContainers.Map                    as SMap
import           System.IO.Unsafe
import           System.Time.Extra                    (duration)

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
    for_ transitiveDirtyKeys $ \k ->
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
    :: forall key value . (RuleResult key ~ value, Typeable key, Show key, Hashable key, Eq key, Typeable value)
    => Database -> [key] -> IO ([Key], [value])
build db keys = do
    (ids, vs) <- runAIO $ fmap unzip $ either return liftIO =<<
            builder db (map Key keys)
    pure (ids, map (asV . resultValue) vs)
    where
        asV :: Value -> value
        asV (Value x) = unwrapDynamic x

-- | Build a list of keys and return their results.
--  If none of the keys are dirty, we can return the results immediately.
--  Otherwise, a blocking computation is returned *which must be evaluated asynchronously* to avoid deadlock.
builder
    :: Database -> [Key] -> AIO (Either [(Key, Result)] (IO [(Key, Result)]))
builder db@Database{..} keys = withRunInIO $ \(RunInIO run) -> do
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
                Running _ force val _ -> do
                    modifyTVar' toForce (Wait force :)
                    pure val
                Dirty s -> do
                    let act = run (refresh db id s)
                        (force, val) = splitIO (join act)
                    SMap.focus (updateStatus $ Running current force val s) id databaseValues
                    modifyTVar' toForce (Spawn force:)
                    pure val

            pure (id, val)

    toForceList <- liftIO $ readTVarIO toForce
    let waitAll = run $ mapConcurrentlyAIO_ id toForceList
    case toForceList of
        [] -> return $ Left results
        _ -> return $ Right $ do
                waitAll
                pure results

-- | Refresh a key:
--     * If no dirty dependencies and we have evaluated the key previously, then we refresh it in the current thread.
--       This assumes that the implementation will be a lookup
--     * Otherwise, we spawn a new thread to refresh the dirty deps (if any) and the key itself
refresh :: Database -> Key -> Maybe Result -> AIO (IO Result)
refresh db key result@(Just me@Result{resultDeps = ResultDeps deps}) = do
    res <- builder db deps
    case res of
      Left res ->
        if isDirty res
            then asyncWithCleanUp $ liftIO $ compute db key RunDependenciesChanged result
            else pure $ compute db key RunDependenciesSame result
      Right iores -> asyncWithCleanUp $ liftIO $ do
        res <- iores
        let mode = if isDirty res then RunDependenciesChanged else RunDependenciesSame
        compute db key mode result
    where
        isDirty = any (\(_,dep) -> resultBuilt me < resultChanged dep)

refresh db key result =
    asyncWithCleanUp $ liftIO $ compute db key RunDependenciesChanged result


-- | Compute a key.
compute :: Database -> Key -> RunMode -> Maybe Result -> IO Result
compute db@Database{..} key mode result = do
    let act = runRule databaseRules key (fmap resultData result) mode
    deps <- newIORef UnknownDeps
    (execution, RunResult{..}) <-
        duration $ runReaderT (fromAction act) $ SAction db deps
    built <- readTVarIO databaseStep
    deps <- readIORef deps
    let changed = if runChanged == ChangedRecomputeDiff then built else maybe built resultChanged result
        built' = if runChanged /= ChangedNothing then built else changed
        -- only update the deps when the rule ran with changes
        actualDeps = if runChanged /= ChangedNothing then deps else previousDeps
        previousDeps= maybe UnknownDeps resultDeps result
    let res = Result runValue built' changed built actualDeps execution runStore
    case getResultDepsDefault [] actualDeps of
        deps | not(null deps)
            && runChanged /= ChangedNothing
                    -> do
            void $ forkIO $
                updateReverseDeps key db
                    (getResultDepsDefault [] previousDeps)
                    (HSet.fromList deps)
        _ -> pure ()
    atomicallyNamed "compute" $ SMap.focus (updateStatus $ Clean res) key databaseValues
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

-- | Returns ann approximation of the database keys,
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
    -> [Key] -- ^ Previous direct dependencies of Id
    -> HashSet Key -- ^ Current direct dependencies of Id
    -> IO ()
updateReverseDeps myId db prev new = uninterruptibleMask_ $ do
    forM_ prev $ \d ->
        unless (d `HSet.member` new) $
            doOne (HSet.delete myId) d
    forM_ (HSet.toList new) $
        doOne (HSet.insert myId)
    where
        alterRDeps f =
            Focus.adjust (onKeyReverseDeps f)
        -- updating all the reverse deps atomically is not needed.
        -- Therefore, run individual transactions for each update
        -- in order to avoid contention
        doOne f id = atomicallyNamed "updateReverseDeps" $
            SMap.focus (alterRDeps f) id (databaseValues db)

getReverseDependencies :: Database -> Key -> STM (Maybe (HashSet Key))
getReverseDependencies db = (fmap.fmap) keyReverseDeps  . flip SMap.lookup (databaseValues db)

transitiveDirtySet :: Foldable t => Database -> t Key -> IO (HashSet Key)
transitiveDirtySet database = flip State.execStateT HSet.empty . traverse_ loop
  where
    loop x = do
        seen <- State.get
        if x `HSet.member` seen then pure () else do
            State.put (HSet.insert x seen)
            next <- lift $ atomically $ getReverseDependencies database x
            traverse_ loop (maybe mempty HSet.toList next)

-- | IO extended to track created asyncs to clean them up when the thread is killed,
--   generalizing 'withAsync'
newtype AIO a = AIO { unAIO :: ReaderT (IORef [Async ()]) IO a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runAIO :: AIO a -> IO a
runAIO (AIO act) = do
    asyncs <- newIORef []
    runReaderT act asyncs `onException` cleanupAsync asyncs

asyncWithCleanUp :: AIO a -> AIO (IO a)
asyncWithCleanUp act = do
    st <- AIO ask
    io <- unliftAIO act
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
cleanupAsync ref = uninterruptibleMask_ $ do
    asyncs <- readIORef ref
    mapM_ (\a -> throwTo (asyncThreadId a) AsyncCancelled) asyncs
    mapM_ waitCatch asyncs

data Wait a
    = Wait {justWait :: !a}
    | Spawn {justWait :: !a}
    deriving Functor

waitOrSpawn :: Wait (IO a) -> IO (Either (IO a) (Async a))
waitOrSpawn (Wait io)  = pure $ Left io
waitOrSpawn (Spawn io) = Right <$> async io

mapConcurrentlyAIO_ :: (a -> IO ()) -> [Wait a] -> AIO ()
mapConcurrentlyAIO_ _ [] = pure ()
mapConcurrentlyAIO_ f [one] = liftIO $ justWait $ fmap f one
mapConcurrentlyAIO_ f many = do
    ref <- AIO ask
    waits <- liftIO $ uninterruptibleMask $ \restore -> do
        waits <- liftIO $ traverse (waitOrSpawn . fmap (restore . f)) many
        let asyncs = rights waits
        liftIO $ atomicModifyIORef'_ ref (asyncs ++)
        return waits
    liftIO $ traverse_ (either id wait) waits
