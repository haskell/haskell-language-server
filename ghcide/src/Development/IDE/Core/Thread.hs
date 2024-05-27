module Development.IDE.Core.Thread
    ( ThreadRun(..), runInThread)
 where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad            (forever)
import           Control.Monad.Cont       (ContT (ContT))

-- Note [Serializing runs in separate thread]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In a lof cases we want to have a separate thread that will serialize the runs of the actions.
-- Like the db writes, session loading in session loader, shake session restarts.
--
-- Originally we used various ways to implement this, but it was hard to maintain and error prone.
-- Moreover, we can not stop these threads uniformly when we are shutting down the server.
--
-- `Development.IDE.Core.Thread` module provides a declarative api to implement this easily.
-- In `ThreadRun` data type:
-- * `tRunWithResource`: is used to create the resources needed to perform the long running action.
-- * `tWorker`: is the action we want to run in separate thread serially.
--
-- runInThread will create a worker thread to run along with the main thread.
-- runInThread provides `resource` created by `tRunWithResource` and a `TQueue` to send the actions to run.
-- The worker thread will serialize the runs of the actions from the TQueue.


data ThreadRun input workerResource resource arg = ThreadRun {
        tRunWithResource ::
            input -- ^ input of running
            -> (workerResource -> resource -> IO ()) -- ^ the long running action need to run with resource
            -> IO (),
        tWorker -- ^ A single action we want to run in separate thread serially
            :: input -- ^ input of running
            -> workerResource -- ^ writer resource
            -> arg -- ^ argument to run
            -> IO ()
}

-- | runInThread
-- Run a long running action with a additional running thread
-- The additional thread will serialize runs of the actions from the TQueue.
-- Return ContT to run the action
runInThread :: ThreadRun input workerResource resource arg -> input -> ContT () IO (resource, TQueue arg)
runInThread ThreadRun{..} ip = ContT $ \f -> do
    tRunWithResource ip $ \w r -> do
        q <- newTQueueIO
        withAsync (writerThread w q) $ \_ -> f (r, q)
    where
        writerThread r q =
            forever $ do
                l <- atomically $ readTQueue q
                tWorker ip r l
