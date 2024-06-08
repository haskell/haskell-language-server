module Development.IDE.Core.WorkerThread
    (withWorkerQueue, blockRunInThread)
 where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.Strict (newBarrier, signalBarrier,
                                            waitBarrier)
import           Control.Monad             (forever)
import           Control.Monad.Cont        (ContT (ContT))

-- Note [Serializing runs in separate thread]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We often want to take long-running actions using some resource that cannot be shared.
-- In this instance it is useful to have a queue of jobs to run using the resource.
-- Like the db writes, session loading in session loader, shake session restarts.
--
-- Originally we used various ways to implement this, but it was hard to maintain and error prone.
-- Moreover, we can not stop these threads uniformly when we are shutting down the server.
--
-- `Development.IDE.Core.WorkerThread` module provides a simple api to implement this easily.
-- * `withWorkerQueue`: accepts an action to run in separate thread and returns a `TQueue` to send the actions to run.
-- * `blockRunInThread` : accepts a `TQueue` and an action to run in separate thread and waits for the result.


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

-- | 'blockRunInThread' queues up an 'IO' action to be run by a worker thread,
-- and then blocks until the result is computed.
blockRunInThread :: TQueue (IO ()) -> IO result -> IO result
blockRunInThread q act = do
    -- Take an action from TQueue, run it and
    -- use barrier to wait for the result
    barrier <- newBarrier
    atomically $ writeTQueue q $ do
        res <- act
        signalBarrier barrier res
    waitBarrier barrier
