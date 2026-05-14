{-
Module : Development.IDE.Core.WorkerThread
Author : @soulomoon
SPDX-License-Identifier: Apache-2.0

Description : This module provides an API for managing worker threads in the IDE.
see Note [Serializing runs in separate thread]
-}
module Development.IDE.Core.WorkerThread
  ( LogWorkerThread (..),
    withWorkerTasks,
    withWorkerQueue,
    readWorkerTask,
    awaitWorkerTask,
    TaskQueue,
    isEmptyTaskQueue,
    writeTaskQueue,
    withWorkerQueueSimple,
    workerTaskQueue,
    TaskRef,
    withWorkerRef,
    workerTaskRef,
    WorkerTasks (..),
  )
where

import           Control.Concurrent.Async  (withAsync)
import           Control.Concurrent.STM
import           Control.Concurrent.Strict (newBarrier, signalBarrier,
                                            waitBarrier)
import           Control.Exception.Safe    (SomeException, finally, throwIO,
                                            try)
import           Control.Monad.Cont        (ContT (ContT))
import           Data.Maybe                (isNothing)
import qualified Data.Text                 as T
import           Ide.Logger

data LogWorkerThread
  = LogThreadEnding !T.Text
  | LogThreadEnded !T.Text
  | LogSingleWorkStarting !T.Text
  | LogSingleWorkEnded !T.Text
  deriving (Show)

instance Pretty LogWorkerThread where
  pretty = \case
    LogThreadEnding t -> "Worker thread ending:" <+> pretty t
    LogThreadEnded t -> "Worker thread ended:" <+> pretty t
    LogSingleWorkStarting t -> "Worker starting a unit of work: " <+> pretty t
    LogSingleWorkEnded t -> "Worker ended a unit of work: " <+> pretty t

{-
Note [Serializing runs in separate thread]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We often want to take long-running actions using some resource that cannot be shared.
In this instance it is useful to have a queue of jobs to run using the resource.
Like the db writes, session loading in session loader, shake session restarts.

Originally we used various ways to implement this, but it was hard to maintain and error prone.
Moreover, we can not stop these threads uniformly when we are shutting down the server.
-}

-- | 'withWorkerQueueSimple' is a simplified version of 'withWorkerQueue'
-- for the common case where the worker function is just 'id'.
withWorkerQueueSimple :: Recorder (WithPriority LogWorkerThread) -> T.Text -> ContT () IO (WorkerTasks STM (IO ()))
withWorkerQueueSimple recorder title = withWorkerQueue recorder title id

data ExitOrTask t = Exit | Task t

-- | 'withWorkerQueue' creates a new 'TQueue', and launches a worker
-- thread which polls the queue for requests and runs the given worker
-- function on them.
withWorkerQueue
  :: Recorder (WithPriority LogWorkerThread)
  -> T.Text
  -> (a -> IO ())
  -> ContT () IO (WorkerTasks STM a)
withWorkerQueue = withWorkerTasks workerTaskQueue

-- | Similar to @withWorkerQueue@, but facilitates squashing actions using some
-- @Semigroup@ semantics.
withWorkerRef
  :: Semigroup a
  => Recorder (WithPriority LogWorkerThread)
  -> T.Text
  -> (a -> IO ())
  -> ContT () IO (WorkerTasks STM a)
withWorkerRef = withWorkerTasks workerTaskRef

withWorkerTasks :: WorkerTasks' STM t a -> Recorder (WithPriority LogWorkerThread) -> T.Text -> (a -> IO ()) -> ContT () IO (WorkerTasks STM a)
withWorkerTasks WorkerTasks'{..} recorder title workerAction = ContT $ \mainAction -> do
  q <- atomically newWorkerTasks'
  -- Use a TMVar as a stop flag to coordinate graceful shutdown.
  -- The worker thread checks this flag before dequeuing each job; if set, it exits immediately,
  -- ensuring that no new work is started after shutdown is requested.
  -- This mechanism is necessary because some downstream code may swallow async exceptions,
  -- making 'cancel' unreliable for stopping the thread in all cases.
  -- If 'cancel' does interrupt the thread (e.g., while blocked in STM or in a cooperative job),
  -- the thread exits immediately and never checks the TMVar; in such cases, the stop flag is redundant.
  b <- newEmptyTMVarIO
  withAsync (writerThread q b) $ \_ -> do
    mainAction (WorkerTasks (addWorkerTask' q) (nullWorkerTasks' q) (tryReadWorkerTask' q))
    -- if we want to debug the exact location the worker swallows an async exception, we can
    -- temporarily comment out the `finally` clause.
        `finally` atomically (putTMVar b ())
    logWith recorder Debug (LogThreadEnding title)
  logWith recorder Debug (LogThreadEnded title)
  where
    writerThread q b =
      -- See above: check stop flag before dequeuing, exit if set, otherwise run next job.
      do
        task <- atomically $ do
          task <- tryReadWorkerTask' q
          isEm <- isEmptyTMVar b
          case (isEm, task) of
            (False, _)   -> return Exit -- stop flag set, exit
            (_, Just t)  -> return $ Task t -- got a task, run it
            (_, Nothing) -> retry -- no task, wait
        case task of
          Exit -> return ()
          Task t -> do
            logWith recorder Debug $ LogSingleWorkStarting title
            workerAction t
            logWith recorder Debug $ LogSingleWorkEnded title
            writerThread q b

readWorkerTask :: WorkerTasks STM a -> STM a
readWorkerTask WorkerTasks {..} = do
  task <- tryReadWorkerTask
  case task of
    Nothing -> retry
    Just t  -> pure t

-- | 'awaitWorkerTask' queues up an 'IO' action to be run by a worker thread,
-- and then blocks until the result is computed. If the action throws an
-- non-async exception, it is rethrown in the calling thread.
awaitWorkerTask :: WorkerTasks STM (IO ()) -> IO r -> IO r
awaitWorkerTask WorkerTasks {..} act = do
  -- Take an action from TQueue, run it and
  -- use barrier to wait for the result
  barrier <- newBarrier
  atomically $ addWorkerTask (try act >>= signalBarrier barrier)
  resultOrException <- waitBarrier barrier
  case resultOrException of
    Left e  -> throwIO (e :: SomeException)
    Right r -> return r

-- | Creating a class that'll only live in this module doesn't have much value.
-- Create a simple struct instead describing the interface of worker tasks.
data WorkerTasks' m t a = WorkerTasks'
  { newWorkerTasks'    :: m (t a)
  , addWorkerTask'     :: t a -> a -> m ()
  , nullWorkerTasks'   :: t a -> m Bool
  , tryReadWorkerTask' :: t a -> m (Maybe a)
  }

-- | Creating a class that'll only live in this module doesn't have much value.
-- Create a simple struct instead describing the interface of worker tasks.
data WorkerTasks m a = WorkerTasks
  { addWorkerTask     :: a -> m ()
  , nullWorkerTasks   :: m Bool
  , tryReadWorkerTask :: m (Maybe a)
  }

-- | Datatype for the queue of jobs mentioned in Note [Serializing runs in separate thread].
data TaskQueue a = TaskQueue (TQueue a)

workerTaskQueue :: WorkerTasks' STM TaskQueue a
workerTaskQueue = WorkerTasks'
  { newWorkerTasks' = TaskQueue <$> newTQueue
  , addWorkerTask' = writeTaskQueue
  , nullWorkerTasks' = isEmptyTaskQueue
  , tryReadWorkerTask' = tryReadTaskQueue
  }

writeTaskQueue :: TaskQueue a -> a -> STM ()
writeTaskQueue (TaskQueue q) = writeTQueue q

isEmptyTaskQueue :: TaskQueue a -> STM Bool
isEmptyTaskQueue (TaskQueue q) = isEmptyTQueue q

tryReadTaskQueue :: TaskQueue a -> STM (Maybe a)
tryReadTaskQueue (TaskQueue q) = tryReadTQueue q

-- | Similar to @TaskQueue@, but facilitates squashing actions using some
-- @Semigroup@ semantics.
newtype TaskRef a = TaskRef (TVar (Maybe a))

workerTaskRef :: Semigroup a => WorkerTasks' STM TaskRef a
workerTaskRef = WorkerTasks'
  { newWorkerTasks' = TaskRef <$> newTVar mempty
  , addWorkerTask' = \(TaskRef t) a -> do
      workerTask <- readTVar t
      case workerTask of
        Nothing -> writeTVar t (Just a)
        Just wt -> writeTVar t (Just (wt <> a))
  , nullWorkerTasks' = \(TaskRef t) -> do
      workerTask <- readTVar t
      pure $ isNothing workerTask
  , tryReadWorkerTask' = \(TaskRef t) -> do
      workerTask <- readTVar t
      writeTVar t Nothing
      pure workerTask
  }
