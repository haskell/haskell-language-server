module Development.IDE.Core.Thread where
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad            (forever)
import           Control.Monad.Cont       (ContT (ContT))


data ThreadRun input threadResource resource arg = ThreadRun {
        tRunWithResource ::
            input -- ^ input of running
            -> (threadResource -> resource -> IO ()) -- ^ the long running action
            -> IO (),
        tWorker -- ^ A single action we want to run in separate thread serially
            :: input -- ^ input of running
            -> threadResource -- ^ writer resource
            -> arg -- ^ argument to run
            -> IO ()
}

-- | runInThread
-- Run a long running action with a additional running thread
-- The additional thread will serialize runs of the actions from the TQueue.
-- Return ContT to run the action
runInThread :: ThreadRun input threadResource resource arg -> input -> ContT () IO (resource, TQueue arg)
runInThread ThreadRun{..} ip = ContT $ \f -> do
    tRunWithResource ip $ \w r -> do
        q <- newTQueueIO
        withAsync (writerThread w q) $ \_ -> f (r, q)
    where
        writerThread r q =
            forever $ do
                l <- atomically $ readTQueue q
                tWorker ip r l
