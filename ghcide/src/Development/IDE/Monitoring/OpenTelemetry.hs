module Development.IDE.Monitoring.OpenTelemetry (monitoring) where

import           Control.Concurrent.Async         (Async, async, cancel)
import           Control.Monad                    (forever)
import           Data.IORef.Extra                 (atomicModifyIORef'_,
                                                   newIORef, readIORef)
import           Data.Text.Encoding               (encodeUtf8)
import           Debug.Trace.Flags                (userTracingEnabled)
import           Development.IDE.Types.Monitoring (Monitoring (..))
import           OpenTelemetry.Eventlog           (mkValueObserver, observe)
import           System.Time.Extra                (Seconds, sleep)

-- | Dump monitoring to the eventlog using the Opentelemetry package
monitoring :: IO Monitoring
monitoring
  | userTracingEnabled = do
    actions <- newIORef []
    let registerCounter name read = do
            observer <- mkValueObserver (encodeUtf8 name)
            let update = observe observer . fromIntegral =<< read
            atomicModifyIORef'_ actions (update :)
        registerGauge = registerCounter
    let start = do
            a <- regularly 1 $ sequence_ =<< readIORef actions
            return (cancel a)
    return Monitoring{..}
  | otherwise = mempty


regularly :: Seconds -> IO () -> IO (Async ())
regularly delay act = async $ forever (act >> sleep delay)
