{-
Module : Development.IDE.Core.WorkerThread
Author : @soulomoon
SPDX-License-Identifier: Apache-2.0

Description : This module provides an API for managing worker threads in the IDE.
see Note [Serializing runs in separate thread]
-}
module Development.IDE.Core.WorkerThread
  ( LogWorkerThread (..),
    withWorkerQueue,
    awaitRunInThread,
    TaskQueue,
    writeTaskQueue,
    withWorkerQueueSimple
  )
where

import           Control.Concurrent.Async  (withAsync)
import           Control.Concurrent.STM
import           Control.Concurrent.Strict (newBarrier, signalBarrier,
                                            waitBarrier)
import           Control.Exception.Safe    (SomeException, finally, throwIO,
                                            try)
import           Control.Monad.Cont        (ContT (ContT))
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
data TaskQueue a = TaskQueue (TQueue a)
newTaskQueueIO :: IO (TaskQueue a)
newTaskQueueIO = TaskQueue <$> newTQueueIO
data ExitOrTask t = Exit | Task t

-- | 'withWorkerQueue' creates a new 'TQueue', and launches a worker
-- thread which polls the queue for requests and runs the given worker
-- function on them.
withWorkerQueueSimple :: Recorder (WithPriority LogWorkerThread) -> T.Text -> ContT () IO (TaskQueue (IO ()))
withWorkerQueueSimple log title = withWorkerQueue log title id
withWorkerQueue :: Recorder (WithPriority LogWorkerThread) -> T.Text -> (t -> IO ()) -> ContT () IO (TaskQueue t)
withWorkerQueue log title workerAction = ContT $ \mainAction -> do
  q <- newTaskQueueIO
  -- Use a TMVar as a stop flag to coordinate graceful shutdown.
  -- The worker thread checks this flag before dequeuing each job; if set, it exits immediately,
  -- ensuring that no new work is started after shutdown is requested.
  -- This mechanism is necessary because some downstream code may swallow async exceptions,
  -- making 'cancel' unreliable for stopping the thread in all cases.
  -- If 'cancel' does interrupt the thread (e.g., while blocked in STM or in a cooperative job),
  -- the thread exits immediately and never checks the TMVar; in such cases, the stop flag is redundant.
  b <- newEmptyTMVarIO
  withAsync (writerThread q b) $ \_ -> do
    mainAction q
    -- if we want to debug the exact location the worker swallows an async exception, we can
    -- temporarily comment out the `finally` clause.
        `finally` atomically (putTMVar b ())
    logWith log Debug (LogThreadEnding title)
  logWith log Debug (LogThreadEnded title)
  where
    -- writerThread :: TaskQueue t -> TMVar () -> (forall a. IO a -> IO a) -> IO ()
    writerThread q b =
      -- See above: check stop flag before dequeuing, exit if set, otherwise run next job.
      do
        task <- atomically $ do
          task <- tryReadTaskQueue q
          isEm <- isEmptyTMVar b
          case (isEm, task) of
            (False, _)   -> return Exit -- stop flag set, exit
            (_, Just t)  -> return $ Task t -- got a task, run it
            (_, Nothing) -> retry -- no task, wait
        case task of
          Exit -> return ()
          Task t -> do
                logWith log Debug $ LogSingleWorkStarting title
                workerAction t
                logWith log Debug $ LogSingleWorkEnded title
                writerThread q b


-- | 'awaitRunInThread' queues up an 'IO' action to be run by a worker thread,
-- and then blocks until the result is computed. If the action throws an
-- non-async exception, it is rethrown in the calling thread.
awaitRunInThread :: TaskQueue (IO ()) -> IO result -> IO result
awaitRunInThread (TaskQueue q) act = do
  -- Take an action from TQueue, run it and
  -- use barrier to wait for the result
  barrier <- newBarrier
  atomically $ writeTQueue q (try act >>= signalBarrier barrier)
  resultOrException <- waitBarrier barrier
  case resultOrException of
    Left e  -> throwIO (e :: SomeException)
    Right r -> return r

writeTaskQueue :: TaskQueue a -> a -> STM ()
writeTaskQueue (TaskQueue q) = writeTQueue q

tryReadTaskQueue :: TaskQueue a -> STM (Maybe a)
tryReadTaskQueue (TaskQueue q) = tryReadTQueue q
