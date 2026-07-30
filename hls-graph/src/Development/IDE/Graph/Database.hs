module Development.IDE.Graph.Database(
    ShakeDatabase,
    ShakeValue,
    shakeNewDatabase,
    shakeNewDatabaseWithRuntime,
    shakeRunDatabase,
    shakeRunDatabaseForKeys,
    shakeRunDatabaseWithExceptions,
    shakeRunDatabaseForKeysWithExceptions,
    shakeRunDatabaseForKeysSep,
    shakeProfileDatabase,
    shakeGetBuildStep,
    shakeGetDatabaseKeys,
    shakeGetDirtySet,
    shakeGetCleanKeys
    ,shakeGetBuildEdges,
    shakeShutDatabase,
    shakeGetActionQueueLength,
    RuntimeRestartKeys(..),
    shakeComputeToPreserve,
    -- shakedatabaseRuntimeDep,
    shakePeekAsyncsDelivers,
    instantiateDelayedAction,
    mkDelayedAction,
    shakeDatabaseSize) where
import           Control.Concurrent.Extra                (Barrier, newBarrier,
                                                          signalBarrier,
                                                          waitBarrierMaybe)
import           Control.Concurrent.STM.Stats            (atomically,
                                                          atomicallyNamed,
                                                          readTVarIO)
import           Control.Exception                       (SomeException,
                                                          throwIO, try)
import           Control.Monad                           (join, unless, void)
import           Control.Monad.IO.Class                  (liftIO)
import           Data.Dynamic
import           Data.Maybe
import           Data.Unique
import           Development.IDE.Graph.Classes           ()
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Profile  (writeProfile)
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import qualified Development.IDE.Graph.Internal.Types    as Logger
import qualified StmContainers.Map                       as SMap


-- Placeholder to be the 'extra' if the user doesn't set it
data NonExportedType = NonExportedType

shakeShutDatabase :: KeySet -> ShakeDatabase -> IO ()
shakeShutDatabase dirties (ShakeDatabase _ _ db) = shutDatabase dirties db

shakeNewDatabase :: ShakeOptions -> Rules () -> IO ShakeDatabase
shakeNewDatabase opts rules = do
    aq <- newQueue
    shakeNewDatabaseWithRuntime (const $ pure ()) aq opts rules

shakeNewDatabaseWithRuntime :: (String -> IO ()) -> ActionQueue -> ShakeOptions -> Rules () -> IO ShakeDatabase
shakeNewDatabaseWithRuntime l aq opts rules = do
    let extra = fromMaybe (toDyn NonExportedType) $ shakeExtra opts
    (theRules, actions) <- runRules extra rules
    db <- newDatabase l aq extra theRules
    pure $ ShakeDatabase (length actions) actions db

shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO [a]
shakeRunDatabase s xs = shakeRunDatabaseForKeys Nothing s xs

shakeRunDatabaseWithExceptions :: ShakeDatabase -> [Action a] -> IO [Either SomeException a]
shakeRunDatabaseWithExceptions s xs = shakeRunDatabaseForKeysWithExceptions Nothing s xs

-- | Returns the set of dirty keys annotated with their age (in # of builds)
shakeGetDirtySet :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDirtySet (ShakeDatabase _ _ db) =
    Development.IDE.Graph.Internal.Database.getDirtySet db

-- | Returns the build number
shakeGetBuildStep :: ShakeDatabase -> IO Int
shakeGetBuildStep (ShakeDatabase _ _ db) = do
    Step s <- readTVarIO $ databaseStep db
    return s

-- Only valid if we never pull on the results, which we don't
unvoid :: Functor m => m () -> m a
unvoid = fmap undefined

-- | Assumes that the database is not running a build
-- The nested IO is to
-- seperate incrementing the step from running the build.
-- Also immediately enqueues upsweep actions for the newly dirty keys.
shakeRunDatabaseForKeysSep
    :: Maybe (RuntimeRestartKeys, KeySet) -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO (IO [Either SomeException a])
shakeRunDatabaseForKeysSep keysChanged sdb@(ShakeDatabase _ as1 db) acts = do
    preserves <- incDatabase db keysChanged
    reenqueued <- atomicallyNamed "actionQueue - peek" $ peekInProgress (databaseActionQueue db)
    let reenqueuedExceptPreserves = filter (\d -> uniqueID d `notMemberKeySet` preserves) reenqueued
    let ignoreResultActs = as1
    return $ do
        seqRunActions (newKey "root") db $ map (pumpActionThreadReRun sdb) reenqueuedExceptPreserves
        drop (length ignoreResultActs) <$> runActions (newKey "root") db (map unvoid ignoreResultActs ++ acts)

instantiateDelayedAction
    :: DelayedAction a
    -> IO (Barrier (Either SomeException a), DelayedActionInternal)
instantiateDelayedAction (DelayedAction u s p a) = do
  b <- newBarrier
  let a' = do
        -- work gets reenqueued when the Shake session is restarted
        -- it can happen that a work item finished just as it was reenqueued
        -- in that case, skipping the work is fine
        alreadyDone <- liftIO $ isJust <$> waitBarrierMaybe b
        unless alreadyDone $ do
          x <- actionCatch @SomeException (Right <$> a) (pure . Left)
          -- ignore exceptions if the barrier has been filled concurrently
          liftIO $ void $ try @SomeException $ signalBarrier b x
      d' = DelayedAction u s p a'
  return (b, d')

mkDelayedAction :: String -> Logger.Priority -> Action a -> IO (DelayedAction a)
mkDelayedAction s p a = do
    u <- newUnique
    return $ DelayedAction (newDirectKey $ hashUnique u) s (toEnum (fromEnum p)) a

shakeComputeToPreserve :: ShakeDatabase -> KeySet -> IO RuntimeRestartKeys
shakeComputeToPreserve (ShakeDatabase _ _ db) ks = atomically (computeToPreserve db ks)

shakeRunDatabaseForKeys
    :: Maybe [Key]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO [a]
shakeRunDatabaseForKeys keysChanged sdb as2 =
    shakeRunDatabaseForKeysWithExceptions keysChanged sdb as2 >>= traverse (either throwIO pure)

shakeRunDatabaseForKeysWithExceptions
    :: Maybe [Key]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO [Either SomeException a]
shakeRunDatabaseForKeysWithExceptions Nothing sdb as2 = join $ shakeRunDatabaseForKeysSep Nothing sdb as2
shakeRunDatabaseForKeysWithExceptions (Just x) sdb as2 =
    let y = fromListKeySet x
        restartKeys = RuntimeRestartKeys
            { restartKillKeys = y
            , restartDirtyKeys = toListKeySet y
            }
    in join $ shakeRunDatabaseForKeysSep (Just (restartKeys, y)) sdb as2


shakePeekAsyncsDelivers :: ShakeDatabase -> IO [DeliverStatus]
shakePeekAsyncsDelivers (ShakeDatabase _ _ db) = peekAsyncsDelivers db

shakeDatabaseSize :: ShakeDatabase -> IO Int
shakeDatabaseSize (ShakeDatabase _ _ db) = databaseSize db

databaseSize :: Database -> IO Int
databaseSize db = atomically $ SMap.size $ databaseValues db

-- | Given a 'ShakeDatabase', write an HTML profile to the given file about the latest run.
shakeProfileDatabase :: ShakeDatabase -> FilePath -> IO ()
shakeProfileDatabase (ShakeDatabase _ _ db) file = writeProfile file db

-- | Returns the clean keys in the database
shakeGetCleanKeys :: ShakeDatabase -> IO [(Key, Result )]
shakeGetCleanKeys (ShakeDatabase _ _ db) = do
    keys <- getDatabaseValues db
    return [ (k,res) | (k, Clean res) <- keys]

-- | Returns the total count of edges in the build graph
shakeGetBuildEdges :: ShakeDatabase -> IO Int
shakeGetBuildEdges (ShakeDatabase _ _ db) = do
    keys <- getDatabaseValues db
    let ress = mapMaybe (getResult . snd) keys
    return $ sum $ map (lengthKeySet . getResultDepsDefault mempty . resultDeps) ress

-- | Returns an approximation of the database keys,
--   annotated with how long ago (in # builds) they were visited
shakeGetDatabaseKeys :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDatabaseKeys (ShakeDatabase _ _ db) = getKeysAndVisitAge db

shakeGetActionQueueLength :: ShakeDatabase -> IO Int
shakeGetActionQueueLength (ShakeDatabase _ _ db) = do
    fromIntegral <$> atomically (countQueue (databaseActionQueue db))
