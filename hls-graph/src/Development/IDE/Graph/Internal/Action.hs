{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Graph.Internal.Action
( ShakeValue
, actionFork
, actionBracket
, actionCatch
, actionFinally
, alwaysRerun
, apply1
, apply
, applyWithoutDependency
, parallel
, runActions
, Development.IDE.Graph.Internal.Action.getDirtySet
, getKeysAndVisitedAge
, isAsyncException
, pumpActionThread
, pumpActionThreadReRun
, sequenceRun
, seqRunActions
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.Stats            (atomicallyNamed)
import           Control.DeepSeq                         (force)
import           Control.Exception
import           Control.Monad                           (void)
import           Control.Monad.IO.Class
import           Control.Monad.RWS                       (MonadReader (ask),
                                                          asks)
import           Control.Monad.Trans.Class
import           Data.Foldable                           (toList)
import           Data.Functor.Identity
import           Data.IORef
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Rules    (RuleResult)
import           Development.IDE.Graph.Internal.Types
import           System.Exit
import           UnliftIO                                (atomically)

type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, NFData a)

-- | Always rerun this rule when dirty, regardless of the dependencies.
alwaysRerun :: Action ()
alwaysRerun = do
    ref <- asks actionDeps
    liftIO $ modifyIORef' ref (AlwaysRerunDeps mempty <>)

-- parallel :: [Action a] -> Action [Either SomeException a]
-- parallel [] = return []
-- parallel xs = do
--     a <- ask
--     deps <- liftIO $ readIORef $ actionDeps a
--     case deps of
--         UnknownDeps ->
--             -- if we are already in the rerun mode, nothing we do is going to impact our state
--             -- runActionInDb "parallel" xs
--             runActionInDb "parallel" xs
--         deps -> error $ "parallel not supported when we have precise dependencies: " ++ show deps

parallel :: [Action a] -> Action [Either SomeException a]
parallel [] = return []
parallel xs = do
    a <- ask
    deps <- liftIO $ readIORef $ actionDeps a
    case deps of
        UnknownDeps -> do
            -- if we are already in the rerun mode, nothing we do is going to impact our state
            -- runActionInDb "parallel" xs
            liftIO $ mapConcurrently (fmap Right . ignoreState a) xs
        deps -> error $ "parallel not supported when we have precise dependencies: " ++ show deps

pumpActionThreadReRun :: ShakeDatabase -> DelayedAction () -> Action ()
pumpActionThreadReRun (ShakeDatabase _ _ db) d = do
        a <- ask
        s <- atomically $ getDataBaseStepInt db
        liftIO $ runInThreadStmInNewThreads db
            (DeliverStatus s (actionName d) (uniqueID d))
            (ignoreState a $ runOne d) (const $ return ())
  where
    runOne d = setActionKey (uniqueID d) $ do
            _ <- getAction d
            liftIO $ atomically $ doneQueue d (databaseActionQueue db)

pumpActionThread :: ShakeDatabase -> (String -> IO ()) -> Action b
pumpActionThread sdb@(ShakeDatabase _ _ db) logMsg = do
    do
        d <- liftIO $ atomicallyNamed "action queue - pop" $ popQueue (databaseActionQueue db)
        pumpActionThreadReRun sdb d
        pumpActionThread sdb logMsg

ignoreState :: SAction -> Action b -> IO b
ignoreState a x = do
    ref <- newIORef mempty
    runActionMonad x a{actionDeps=ref}

actionFork :: Action a -> (Async a -> Action b) -> Action b
actionFork act k = do
    a <- ask
    deps <- liftIO $ readIORef $ actionDeps a
    let db = actionDatabase a
    case deps of
        UnknownDeps -> do
            [res] <- liftIO $ withAsync (ignoreState a act) $ \as ->
                runActions (actionKey a) db [k as]
            liftIO $ either throwIO pure res
        _ ->
            error "actionFork is only supported when dependencies are unknown"

isAsyncException :: SomeException -> Bool
isAsyncException e
    | Just (_ :: SomeAsyncException) <- fromException e = True
    | Just (_ :: AsyncCancelled) <- fromException e = True
    | Just (_ :: AsyncException) <- fromException e = True
    | Just (_ :: AsyncParentKill) <- fromException e = True
    | Just (_ :: ExitCode) <- fromException e = True
    | otherwise = False


actionCatch :: Exception e => Action a -> (e -> Action a) -> Action a
actionCatch a b = do
    v <- ask
    liftIO $ catchJust f (runActionMonad a v) (\x -> runActionMonad (b x) v)
    where
        -- Catch only catches exceptions that were caused by this code, not those that
        -- are a result of program termination
        f e | isAsyncException e = Nothing
            | otherwise = fromException e

actionBracket :: IO a -> (a -> IO b) -> (a -> Action c) -> Action c
actionBracket a b c = do
    v <- ask
    liftIO $ bracket a b (\x -> runActionMonad (c x) v)

actionFinally :: Action a -> IO b -> Action a
actionFinally a b = do
    v <- Action ask
    Action $ lift $ finally (runActionMonad a v) b

apply1 :: (RuleResult key ~ value, ShakeValue key, Typeable value) => key -> Action value
apply1 k = runIdentity <$> apply (Identity k)

apply :: (Traversable f, RuleResult key ~ value, ShakeValue key, Typeable value) => f key -> Action (f value)
apply ks = do
    db <- asks actionDatabase
    stack <- asks actionStack
    pk <- getActionKey
    (is, vs) <- liftIO $ build pk db stack ks
    ref <- asks actionDeps
    let !ks = force $ fromListKeySet $ toList is
    liftIO $ modifyIORef' ref (ResultDeps [ks] <>)
    pure vs

-- | Evaluate a list of keys without recording any dependencies.
applyWithoutDependency :: (Traversable f, RuleResult key ~ value, ShakeValue key, Typeable value) => f key -> Action (f value)
applyWithoutDependency ks = do
    db <- asks actionDatabase
    stack <- asks actionStack
    pk <- getActionKey
    (_, vs) <- liftIO $ build pk db stack ks
    pure vs

runActions :: Key -> Database -> [Action a] -> IO [Either SomeException a]
runActions pk db xs = do
    deps <- newIORef mempty
    runActionMonad (parallel xs) $ SAction pk db deps emptyStack

seqRunActions :: Key -> Database -> [Action a] -> IO ()
seqRunActions pk db xs = do
    deps <- newIORef mempty
    runActionMonad (sequenceRun xs) $ SAction pk db deps emptyStack

sequenceRun :: [Action a] -> Action ()
sequenceRun [] = return ()
sequenceRun (x:xs) = do
    void x
    sequenceRun xs

-- | Returns the set of dirty keys annotated with their age (in # of builds)
getDirtySet  :: Action [(Key, Int)]
getDirtySet = do
    db <- getDatabase
    liftIO $ Development.IDE.Graph.Internal.Database.getDirtySet db

getKeysAndVisitedAge :: Action [(Key, Int)]
getKeysAndVisitedAge = do
    db <- getDatabase
    liftIO $ Development.IDE.Graph.Internal.Database.getKeysAndVisitAge db
