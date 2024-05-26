module Development.IDE.Core.Thread where
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad            (forever)


data ThreadRun input threadResource resource arg = ThreadRun {
        tCreateResource ::
            input -- ^ input of running
            -> (threadResource -> resource -> IO ()) -- ^ the long running action
            -> IO (),
        tRunner -- ^ run a single action with writer resource
            :: input -- ^ input of running
            -> threadResource -- ^ writer resource
            -> arg -- ^ argument to run
            -> IO ()
}

runInThread :: ThreadRun input threadResource resource arg -> input -> ((resource, TQueue arg) -> IO ()) -> IO ()
runInThread ThreadRun{..} ip f = do
    tCreateResource ip $ \w r -> do
        q <- newTQueueIO
        withAsync (writerThread w q) $ \_ -> f (r, q)
    where
        writerThread r q =
            forever $ do
                l <- atomically $ readTQueue q
                tRunner ip r l
