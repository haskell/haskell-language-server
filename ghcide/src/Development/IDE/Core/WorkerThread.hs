{-
Module : Development.IDE.Core.WorkerThread
Author : @soulomoon
SPDX-License-Identifier: Apache-2.0

Description : This module provides an API for managing worker threads in the IDE.
see Note [Serializing runs in separate thread]
-}
module Development.IDE.Core.WorkerThread
    (withWorkerQueue, awaitRunInThread)
 where

import           Control.Concurrent.Async  (withAsync)
import           Control.Concurrent.STM
import           Control.Concurrent.Strict (newBarrier, signalBarrier,
                                            waitBarrier)
import           Control.Monad             (forever)
import           Control.Monad.Cont        (ContT (ContT))

{-
Note [Serializing runs in separate thread]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We often want to take long-running actions using some resource that cannot be shared.
In this instance it is useful to have a queue of jobs to run using the resource.
Like the db writes, session loading in session loader, shake session restarts.

Originally we used various ways to implement this, but it was hard to maintain and error prone.
Moreover, we can not stop these threads uniformly when we are shutting down the server.
-}

-- | 'withWorkerQueue' creates a new 'TQueue', and launches a worker
-- thread which polls the queue for requests and runs the given worker
-- function on them.
withWorkerQueue :: (t -> IO a) -> ContT () IO (TQueue t)
withWorkerQueue workerAction = ContT $ \mainAction -> do
    q <- newTQueueIO
    withAsync (writerThread q) $ \_ -> mainAction q
    where
        writerThread q =
            forever $ do
                l <- atomically $ readTQueue q
                workerAction l

-- | 'awaitRunInThread' queues up an 'IO' action to be run by a worker thread,
-- and then blocks until the result is computed.
awaitRunInThread :: TQueue (IO ()) -> IO result -> IO result
awaitRunInThread q act = do
    -- Take an action from TQueue, run it and
    -- use barrier to wait for the result
    barrier <- newBarrier
    atomically $ writeTQueue q $ do
        res <- act
        signalBarrier barrier res
    waitBarrier barrier
