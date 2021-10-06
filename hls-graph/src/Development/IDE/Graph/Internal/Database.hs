-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Development.IDE.Graph.Internal.Database (newDatabase, incDatabase, build) where

import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict      as State
import           Data.Dynamic
import           Data.Either
import           Data.Foldable                         (traverse_)
import           Data.IORef.Extra
import           Data.IntSet                           (IntSet)
import qualified Data.IntSet                           as Set
import           Data.Maybe
import           Data.Tuple.Extra
import           Development.IDE.Graph.Classes
import qualified Development.IDE.Graph.Internal.Ids    as Ids
import           Development.IDE.Graph.Internal.Intern
import qualified Development.IDE.Graph.Internal.Intern as Intern
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import           System.IO.Unsafe
import           System.Time.Extra                     (duration)

newDatabase :: Dynamic -> TheRules -> IO Database
newDatabase databaseExtra databaseRules = do
    databaseStep <- newIORef $ Step 0
    databaseLock <- newLock
    databaseIds <- newIORef Intern.empty
    databaseValues <- Ids.empty
    databaseReverseDeps <- Ids.empty
    databaseReverseDepsLock <- newLock
    databaseDirtySet <- newIORef Nothing
    pure Database{..}

-- | Increment the step and mark dirty
incDatabase :: Database -> Maybe [Key] -> IO ()
-- all keys are dirty
incDatabase db Nothing = do
    modifyIORef' (databaseStep db) $ \(Step i) -> Step $ i + 1
    writeIORef (databaseDirtySet db) Nothing
    withLock (databaseLock db) $
        Ids.forMutate (databaseValues db) $ \_ -> second $ \case
            Clean x       -> Dirty (Just x)
            Dirty x       -> Dirty x
            Running _ _ x -> Dirty x
-- only some keys are dirty
incDatabase db (Just kk) = do
    modifyIORef' (databaseStep db) $ \(Step i) -> Step $ i + 1
    intern <- readIORef (databaseIds db)
    let dirtyIds = mapMaybe (`Intern.lookup` intern) kk
    transitiveDirtyIds <- transitiveDirtySet db dirtyIds
    modifyIORef (databaseDirtySet db) (\dd -> Just $ fromMaybe mempty dd <> transitiveDirtyIds)
    withLock (databaseLock db) $
        Ids.forMutate (databaseValues db) $ \i -> \case
            (k, Running _ _ x) -> (k, Dirty x)
            (k, Clean x) | i `Set.member` transitiveDirtyIds ->
                (k, Dirty (Just x))
            other -> other


-- | Unwrap and build a list of keys in parallel
build
    :: forall key value . (RuleResult key ~ value, Typeable key, Show key, Hashable key, Eq key, Typeable value)
    => Database -> [key] -> IO ([Id], [value])
build db keys = do
    (ids, vs) <- runAIO $ fmap unzip $ either return liftIO =<< builder db (map (Right . Key) keys)
    pure (ids, map (asV . resultValue) vs)
    where
        asV :: Value -> value
        asV (Value x) = unwrapDynamic x

-- | Build a list of keys and return their results.
--  If none of the keys are dirty, we can return the results immediately.
--  Otherwise, a blocking computation is returned *which must be evaluated asynchronously* to avoid deadlock.
builder
    :: Database -> [Either Id Key] -> AIO (Either [(Id, Result)] (IO [(Id, Result)]))
builder db@Database{..} keys = do
        -- Things that I need to force before my results are ready
        toForce <- liftIO $ newIORef []

        results <- withLockAIO databaseLock $ do
            flip traverse keys $ \idKey -> do
                -- Resolve the id
                id <- case idKey of
                    Left id -> pure id
                    Right key -> liftIO $ do
                        ids <- readIORef databaseIds
                        case Intern.lookup key ids of
                            Just v -> pure v
                            Nothing -> do
                                (ids, id) <- pure $ Intern.add key ids
                                writeIORef' databaseIds ids
                                return id

                -- Spawn the id if needed
                status <- liftIO $ Ids.lookup databaseValues id
                val <- case fromMaybe (fromRight undefined idKey, Dirty Nothing) status of
                    (_, Clean r) -> pure r
                    (_, Running force val _) -> do
                        liftIO $ modifyIORef toForce (Wait force :)
                        pure val
                    (key, Dirty s) -> do
                        act <- unliftAIO (refresh db key id s)
                        let (force, val) = splitIO (join act)
                        liftIO $ Ids.insert databaseValues id (key, Running force val s)
                        liftIO $ modifyIORef toForce (Spawn force:)
                        pure val

                pure (id, val)

        toForceList <- liftIO $ readIORef toForce
        waitAll <- unliftAIO $ mapConcurrentlyAIO_ id toForceList
        case toForceList of
            [] -> return $ Left results
            _ -> return $ Right $ do
                    waitAll
                    pure results

-- | Refresh a key:
--     * If no dirty dependencies and we have evaluated the key previously, then we refresh it in the current thread.
--       This assumes that the implementation will be a lookup
--     * Otherwise, we spawn a new thread to refresh the dirty deps (if any) and the key itself
refresh :: Database -> Key -> Id -> Maybe Result -> AIO (IO Result)
refresh db key id result@(Just me@Result{resultDeps=Just deps}) = do
    res <- builder db $ map Left deps
    case res of
      Left res ->
        if isDirty res
            then asyncWithCleanUp $ liftIO $ compute db key id RunDependenciesChanged result
            else pure $ compute db key id RunDependenciesSame result
      Right iores -> asyncWithCleanUp $ liftIO $ do
        res <- iores
        let mode = if isDirty res then RunDependenciesChanged else RunDependenciesSame
        compute db key id mode result
    where
        isDirty = any (\(_,dep) -> resultBuilt me < resultChanged dep)

refresh db key id result =
    asyncWithCleanUp $ liftIO $ compute db key id RunDependenciesChanged result


-- | Compute a key.
compute :: Database -> Key -> Id -> RunMode -> Maybe Result -> IO Result
compute db@Database{..} key id mode result = do
    let act = runRule databaseRules key (fmap resultData result) mode
    deps <- newIORef $ Just []
    (execution, RunResult{..}) <-
        duration $ runReaderT (fromAction act) $ SAction db deps
    built <- readIORef databaseStep
    deps <- readIORef deps
    let changed = if runChanged == ChangedRecomputeDiff then built else maybe built resultChanged result
        built' = if runChanged /= ChangedNothing then built else changed
        -- only update the deps when the rule ran with changes
        actualDeps = if runChanged /= ChangedNothing then deps else previousDeps
        previousDeps= resultDeps =<< result
    let res = Result runValue built' changed built actualDeps execution runStore
    case actualDeps of
        Just deps | not(null deps) &&
                    runChanged /= ChangedNothing
                    -> do
            void $ forkIO $
                updateReverseDeps id db (fromMaybe [] previousDeps) (Set.fromList deps)
        _ -> pure ()
    withLock databaseLock $
        Ids.insert databaseValues id (key, Clean res)
    pure res

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
    :: Id         -- ^ Id
    -> Database
    -> [Id] -- ^ Previous direct dependencies of Id
    -> IntSet     -- ^ Current direct dependencies of Id
    -> IO ()
updateReverseDeps myId db prev new = withLock (databaseReverseDepsLock db) $ uninterruptibleMask_ $ do
    forM_ prev $ \d ->
        unless (d `Set.member` new) $
            doOne (Set.delete myId) d
    forM_ (Set.elems new) $
        doOne (Set.insert myId)
    where
        doOne f id = do
            rdeps <- getReverseDependencies db id
            Ids.insert (databaseReverseDeps db) id (f $ fromMaybe mempty rdeps)

getReverseDependencies :: Database -> Id -> IO (Maybe (IntSet))
getReverseDependencies db = Ids.lookup (databaseReverseDeps db)

transitiveDirtySet :: Foldable t => Database -> t Id -> IO IntSet
transitiveDirtySet database = flip State.execStateT Set.empty . traverse_ loop
  where
    loop x = do
        seen <- State.get
        if x `Set.member` seen then pure () else do
            State.put (Set.insert x seen)
            next <- lift $ getReverseDependencies database x
            traverse_ loop (maybe mempty Set.toList next)

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

withLockAIO :: Lock -> AIO a -> AIO a
withLockAIO lock act = do
    io <- unliftAIO act
    liftIO $ withLock lock io

unliftAIO :: AIO a -> AIO (IO a)
unliftAIO act = do
    st <- AIO ask
    return $ runReaderT (unAIO act) st

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
        waits <- liftIO $ traverse waitOrSpawn (map (fmap (restore . f)) many)
        let asyncs = rights waits
        liftIO $ atomicModifyIORef'_ ref (asyncs ++)
        return waits
    liftIO $ traverse_ (either id wait) waits
