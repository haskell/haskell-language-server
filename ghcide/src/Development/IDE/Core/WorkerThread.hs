{-
Module : Development.IDE.Core.WorkerThread
Author : @soulomoon
SPDX-License-Identifier: Apache-2.0

Description : This module provides an API for managing worker threads in the IDE.
see Note [Serializing runs in separate thread]
-}
module Development.IDE.Core.WorkerThread
    (withWorkerQueue
    , awaitRunInThread
    , withWorkerQueueOfOne
    , WorkerQueue
    , writeWorkerQueue
    , waitUntilWorkerQueueEmpty)
 where

import           Control.Concurrent.Async  (withAsync)
import           Control.Concurrent.STM
import           Control.Concurrent.Strict (newBarrier, signalBarrier,
                                            waitBarrier)
import           Control.Exception         (finally)
import           Control.Monad             (forever, unless)
import           Control.Monad.Cont        (ContT (ContT))
import           Control.Monad.IO.Class    (liftIO)

{-
Note [Serializing runs in separate thread]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We often want to take long-running actions using some resource that cannot be shared.
In this instance it is useful to have a queue of jobs to run using the resource.
Like the db writes, session loading in session loader, shake session restarts.

Originally we used various ways to implement this, but it was hard to maintain and error prone.
Moreover, we can not stop these threads uniformly when we are shutting down the server.
-}

data WorkerQueue a = WorkerQueueOfOne (TMVar a) | WorkerQueueOfMany (TQueue a)

writeWorkerQueue :: WorkerQueue a -> a -> STM ()
writeWorkerQueue (WorkerQueueOfOne tvar) action    = putTMVar tvar action
writeWorkerQueue (WorkerQueueOfMany tqueue) action = writeTQueue tqueue action

newWorkerQueue :: STM (WorkerQueue a)
newWorkerQueue = WorkerQueueOfMany <$> newTQueue

newWorkerQueueOfOne :: STM (WorkerQueue a)
newWorkerQueueOfOne = WorkerQueueOfOne <$> newEmptyTMVar


-- | 'withWorkerQueue' creates a new 'WorkerQueue', and launches a worker
-- thread which polls the queue for requests and runs the given worker
-- function on them.
withWorkerQueue :: (t -> IO a) -> ContT () IO (WorkerQueue t)
withWorkerQueue workerAction = do
    q <- liftIO $ atomically newWorkerQueue
    runWorkerQueue q workerAction

-- | 'withWorkerQueueOfOne' creates a new 'WorkerQueue' that only allows one action to be queued at a time.
-- and one action can only be queued after the previous action has been done.
-- this is useful when we want to cancel the action waiting to be enqueue if it's thread is cancelled.
-- e.g. session loading in session loader. When a shake session is restarted, we want to cancel the previous pending session loading.
withWorkerQueueOfOne :: (t -> IO a) -> ContT () IO (WorkerQueue t)
withWorkerQueueOfOne workerAction = do
    q <- liftIO $ atomically newWorkerQueueOfOne
    runWorkerQueue q workerAction

runWorkerQueue :: WorkerQueue t -> (t -> IO a) -> ContT () IO (WorkerQueue t)
runWorkerQueue q workerAction = ContT $ \mainAction -> do
    withAsync (writerThread q) $ \_ -> mainAction q
    where
        writerThread q =
            forever $ do
                case q of
                    -- only remove the action from the queue after it has done
                    WorkerQueueOfOne tvar -> do
                        l <- atomically $ readTMVar tvar
                        workerAction l `finally` atomically (takeTMVar tvar)
                    WorkerQueueOfMany q -> do
                        l <- atomically $ peekTQueue q
                        workerAction l `finally` atomically (readTQueue q)

-- | waitUntilWorkerQueueEmpty blocks until the worker queue is empty.
waitUntilWorkerQueueEmpty :: WorkerQueue a -> IO ()
waitUntilWorkerQueueEmpty (WorkerQueueOfOne tvar) = atomically $ do
    isEmpty <- isEmptyTMVar tvar
    unless isEmpty retry
waitUntilWorkerQueueEmpty (WorkerQueueOfMany queue) = atomically $ do
    isEmpty <- isEmptyTQueue queue
    unless isEmpty retry

-- | 'awaitRunInThread' queues up an 'IO' action to be run by a worker thread,
-- and then blocks until the result is computed.
awaitRunInThread :: WorkerQueue (IO ()) -> IO result -> IO result
awaitRunInThread q act = do
    -- Take an action from TQueue, run it and
    -- use barrier to wait for the result
    barrier <- newBarrier
    atomically $ writeWorkerQueue q $ do
        res <- act
        signalBarrier barrier res
    waitBarrier barrier
