-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Development.IDE.Graph.Internal.Database where

import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad
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
import qualified Development.IDE.Graph.Internal.Ids    as Ids
import           Development.IDE.Graph.Internal.Intern
import qualified Development.IDE.Graph.Internal.Intern as Intern
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import qualified Development.Shake                     as Shake
import           Development.Shake.Classes
import qualified Development.Shake.Rule                as Shake
import           System.IO.Unsafe

newDatabase :: Dynamic -> TheRules -> IO Database
newDatabase databaseExtra databaseRules = do
    databaseStep <- newIORef $ Step 0
    databaseLock <- newLock
    databaseIds <- newIORef Intern.empty
    databaseValues <- Ids.empty
    databaseReverseDeps <- Ids.empty
    databaseReverseDepsLock <- newLock
    pure Database{..}

-- | Increment the step and mark dirty
incDatabase :: Database -> Maybe [Key] -> IO ()
-- all keys are dirty
incDatabase db Nothing = do
    modifyIORef' (databaseStep db) $ \(Step i) -> Step $ i + 1
    withLock (databaseLock db) $
        Ids.forMutate (databaseValues db) $ \_ -> second $ \case
            Clean x     -> Dirty (Just x)
            Dirty x     -> Dirty x
            Running _ x -> Dirty x
-- only some keys are dirty
incDatabase db (Just kk) = do
    modifyIORef' (databaseStep db) $ \(Step i) -> Step $ i + 1
    intern <- readIORef (databaseIds db)
    let dirtyIds = mapMaybe (`Intern.lookup` intern) kk
    transitiveDirtyIds <- transitiveDirtySet db dirtyIds
    withLock (databaseLock db) $
        Ids.forMutate (databaseValues db) $ \i -> \case
            (k, Running _ x) -> (k, Dirty x)
            (k, Clean x) | i `Set.member` transitiveDirtyIds ->
                (k, Dirty (Just x))
            other -> other


-- | Unwrap and build a list of keys in parallel
build
    :: forall key value . (Shake.RuleResult key ~ value, Typeable key, Show key, Hashable key, Eq key, Typeable value)
    => Database -> [key] -> IO ([Id], [value])
build db keys = do
    (ids, vs) <- fmap unzip $ builder db $ map (Right . Key) keys
    pure (ids, map (asV . resultValue) vs)
    where
        asV :: Value -> value
        asV (Value x) = unwrapDynamic x

-- | Build a list of keys in parallel
builder
    :: Database -> [Either Id Key] -> IO [(Id, Result)]
builder db@Database{..} keys = do
    -- Async things that I own and am responsible for killing
    ownedAsync <- newIORef []
    flip onException (cleanupAsync ownedAsync) $ do

        -- Things that I need to force before my results are ready
        toForce <- newIORef []

        results <- withLock databaseLock $ do
            forM keys $ \idKey -> do
                -- Resolve the id
                id <- case idKey of
                    Left id -> pure id
                    Right key -> do
                        ids <- readIORef databaseIds
                        case Intern.lookup key ids of
                            Just v -> pure v
                            Nothing -> do
                                (ids, id) <- pure $ Intern.add key ids
                                writeIORef' databaseIds ids
                                return id

                -- Spawn the id if needed
                status <- Ids.lookup databaseValues id
                val <- case fromMaybe (fromRight undefined idKey, Dirty Nothing) status of
                    (_, Clean r) -> pure r
                    (_, Running act _) -> do
                        -- we promise to force everything in todo before reading the results
                        -- so the following unsafePerformIO isn't actually unsafe
                        let (force, val) = splitIO act
                        modifyIORef toForce (force:)
                        pure val
                    (key, Dirty s) -> do
                        -- Important we don't lose any Async things we create
                        act <- uninterruptibleMask $ \restore -> do
                            -- the child actions should always be spawned unmasked
                            -- or they can't be killed
                            async <- async $ restore $ check db key id s
                            modifyIORef ownedAsync (async:)
                            pure $ wait async
                        Ids.insert databaseValues id (key, Running act s)
                        let (force, val) = splitIO act
                        modifyIORef toForce (force:)
                        pure val

                pure (id, val)

        sequence_ =<< readIORef toForce
        pure results

cleanupAsync :: IORef [Async a] -> IO ()
cleanupAsync ref = uninterruptibleMask_ $ do
    asyncs <- readIORef ref
    mapM_ (\a -> throwTo (asyncThreadId a) AsyncCancelled) asyncs
    mapM_ waitCatch asyncs

-- | Check if we need to run the database.
check :: Database -> Key -> Id -> Maybe Result -> IO Result
check db key id result@(Just me@Result{resultDeps=Just deps}) = do
    res <- builder db $ map Left deps
    let dirty = any (\(_,dep) -> resultBuilt me < resultChanged dep) res
    let mode = if dirty then Shake.RunDependenciesChanged else Shake.RunDependenciesSame
    spawn db key id mode result
check db key id result = spawn db key id Shake.RunDependenciesChanged result


-- | Spawn a new computation to run the action.
spawn :: Database -> Key -> Id -> Shake.RunMode -> Maybe Result -> IO Result
spawn db@Database{..} key id mode result = do
    let act = runRule databaseRules key (fmap resultData result) mode
    deps <- newIORef $ Just []
    Shake.RunResult{..} <- runReaderT (fromAction act) $ SAction db deps
    built <- readIORef databaseStep
    deps <- readIORef deps
    let changed = if runChanged == Shake.ChangedRecomputeDiff then built else maybe built resultChanged result
        -- only update the deps when the rule ran with changes
    let actualDeps = if runChanged /= Shake.ChangedNothing then deps else previousDeps
        previousDeps= resultDeps =<< result
    let res = Result runValue built changed actualDeps runStore
    withLock databaseLock $ do
        -- recompute reverse deps only when the rule ran with changes
        when (runChanged /= Shake.ChangedNothing) $
          updateReverseDeps id db (fromMaybe [] previousDeps) (maybe mempty Set.fromList actualDeps)
        Ids.insert databaseValues id (key, Clean res)
    pure res

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
updateReverseDeps myId db prev new = withLock (databaseReverseDepsLock db) $ do
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
