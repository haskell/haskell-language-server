{-# LANGUAGE GADTs #-}

module Development.IDE.Plugin.LSPWindowShowMessageRecorder (makeLspShowMessageRecorder) where

import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.IORef
import Data.IORef.Extra (atomicModifyIORef'_)
import Data.Text (Text)
import Development.IDE.Types.Logger
import Ide.Types (PluginDescriptor (pluginNotificationHandlers), defaultPluginDescriptor, mkPluginNotificationHandler)
import Language.LSP.Server (LanguageContextEnv, getLspEnv)
import qualified Language.LSP.Server as LSP
import Language.LSP.Types (MessageType (..), SMethod (SInitialized, SWindowShowMessage), ShowMessageParams (..))

-- | Creates a recorder that logs to the LSP stream via WindowShowMessage notifications.
--   The recorder won't attempt to send messages until the LSP stream is initialized.
makeLspShowMessageRecorder ::
  IO (Recorder (WithPriority Text), PluginDescriptor c)
makeLspShowMessageRecorder = do
  envRef <- newIORef Nothing
  -- messages logged before the LSP stream is initialized will be sent when it is
  backLogRef <- newIORef []
  let recorder = Recorder $ \it -> do
        mbenv <- liftIO $ readIORef envRef
        liftIO $ case mbenv of
          Nothing -> atomicModifyIORef'_ backLogRef (it :)
          Just env -> sendMsg env it
      -- the plugin captures the language context, so it can be used to send messages
      plugin =
        (defaultPluginDescriptor "LSPWindowShowMessageRecorder")
          { pluginNotificationHandlers = mkPluginNotificationHandler SInitialized $ \_ _ _ -> do
              env <- getLspEnv
              liftIO $ writeIORef envRef $ Just env
              -- flush the backlog
              backLog <- liftIO $ atomicModifyIORef' backLogRef ([],)
              liftIO $ for_ (reverse backLog) $ sendMsg env
          }
  return (recorder, plugin)

sendMsg :: LanguageContextEnv config -> WithPriority Text -> IO ()
sendMsg env WithPriority {..} =
  LSP.runLspT env $
    LSP.sendNotification
      SWindowShowMessage
      ShowMessageParams
        { _xtype = priorityToLsp priority,
          _message = payload
        }

priorityToLsp :: Priority -> MessageType
priorityToLsp =
  \case
    Debug -> MtLog
    Info -> MtInfo
    Warning -> MtWarning
    Error -> MtError
