module Development.IDE.Monitoring.EKG(monitoring) where
import           Control.Concurrent               (killThread)
import           Control.Concurrent.Async         (async, waitCatch)
import           Data.Text                        (pack)
import           Development.IDE.Types.Logger     (Logger, logInfo)
import           Development.IDE.Types.Monitoring (Monitoring (..))
import qualified System.Metrics                   as Monitoring
import qualified System.Remote.Monitoring.Wai     as Monitoring

-- | Monitoring using EKG
monitoring :: Logger -> Int -> IO Monitoring
monitoring logger port = do
    store <- Monitoring.newStore
    Monitoring.registerGcMetrics store
    let registerCounter name read = Monitoring.registerGauge name read store
        registerGauge name read = Monitoring.registerGauge name read store
        start = do
            server <- do
                let startServer = Monitoring.forkServerWith store "localhost" port
                -- this can fail if the port is busy, throwing an async exception back to us
                -- to handle that, wrap the server thread in an async
                mb_server <- async startServer >>= waitCatch
                case mb_server of
                    Right s -> do
                        logInfo logger $ pack $
                            "Started monitoring server on port " <> show port
                        return $ Just s
                    Left e -> do
                        logInfo logger $ pack $
                            "Unable to bind monitoring server on port "
                            <> show port <> ":" <> show e
                        return Nothing
            return $ mapM_ (killThread . Monitoring.serverThreadId) server
    return $ Monitoring {..}
