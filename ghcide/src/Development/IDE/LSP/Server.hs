{-# LANGUAGE UndecidableInstances #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.LSP.Server
  ( ReactorMessage (..)
  , ReactorChan
  , PendingRequests (..)
  , newPendingRequests
  , ServerEnv (..)
  , ServerM (..)
  , requestHandler
  , notificationHandler
  ) where
import           Control.Concurrent.STM        (STM, atomically, modifyTVar',
                                                newTVarIO, readTVar, stateTVar)
import           Control.Monad                 (when)
import           Control.Monad.IO.Unlift       (MonadUnliftIO)
import           Control.Monad.Reader
import qualified Data.Map.Strict               as Map
import           Development.IDE.Core.Shake
import           Development.IDE.Core.Tracing
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Server           (Handlers, LspM)
import qualified Language.LSP.Server           as LSP
import           Language.LSP.VFS
import           UnliftIO.Chan
import           UnliftIO.Exception            (mask_)

data ReactorMessage
  = ReactorNotification (IO ())
  | forall m . ReactorRequest (LspId m) (IO ()) (TResponseError m -> IO ())

type ReactorChan = Chan ReactorMessage

data PendingRequests = PendingRequests
    { addPendingRequest    :: SomeLspId -> STM ()
    , cancelPendingRequest :: SomeLspId -> STM ()
    , removePendingRequest :: SomeLspId -> STM Bool
    , isRequestCancelled   :: SomeLspId -> STM Bool
    }

newPendingRequests :: IO PendingRequests
newPendingRequests = do
  pending <- newTVarIO Map.empty
  pure $ PendingRequests
    { addPendingRequest = \sid -> modifyTVar' pending $ Map.insert sid False
    , cancelPendingRequest = \sid ->
        -- We want to avoid that the list of cancelled requests
        -- keeps growing if we receive cancellations for requests
        -- that do not exist or have already been processed.
        modifyTVar' pending $ Map.adjust (const True) sid
    , removePendingRequest = \sid -> stateTVar pending $ \m -> (Map.member sid m, Map.delete sid m)
    , isRequestCancelled = \sid -> Map.findWithDefault False sid <$> readTVar pending
    }

data ServerEnv = ServerEnv
  { serverEnvChannel :: ReactorChan
  , serverEnvPending :: PendingRequests
  , serverEnvState   :: IdeState
  }

newtype ServerM c a = ServerM { unServerM :: ReaderT ServerEnv (LspM c) a }
  deriving (Functor, Applicative, Monad, MonadReader ServerEnv, MonadIO, MonadUnliftIO, LSP.MonadLsp c)

requestHandler
  :: forall m c. PluginMethod Request m =>
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c (Either (TResponseError m) (MessageResult m)))
  -> Handlers (ServerM c)
requestHandler m k = LSP.requestHandler m $ \TRequestMessage{_method,_id,_params} resp -> do
  st@(ServerEnv chan pending ide) <- ask
  env <- LSP.getLspEnv
  let sid = SomeLspId _id
      resp' :: Either (TResponseError m) (MessageResult m) -> LspM c ()
      resp' r =
        -- This has to be masked, otherwise the handler might get cancelled after
        -- the request was deleted from the pending set, causing us to forget to
        -- send a response.
        mask_ $ do
          wasPending <- liftIO $ atomically $ removePendingRequest pending sid
          when wasPending $ (runReaderT . unServerM) (resp r) st
      trace x = otTracedHandler "Request" (show _method) $ \sp -> do
        traceWithSpan sp _params
        x
  liftIO $ atomically $ addPendingRequest pending sid
  writeChan chan $ ReactorRequest _id (trace $ LSP.runLspT env $ resp' =<< k ide _params) (LSP.runLspT env . resp' . Left)

notificationHandler
  :: forall m c. PluginMethod Notification m =>
     SMethod m
  -> (IdeState -> VFS -> MessageParams m -> LspM c ())
  -> Handlers (ServerM c)
notificationHandler m k = LSP.notificationHandler m $ \TNotificationMessage{_params,_method}-> do
  ServerEnv chan _ ide <- ask
  env <- LSP.getLspEnv
  -- Take a snapshot of the VFS state on every notification
  -- We only need to do this here because the VFS state is only updated
  -- on notifications
  vfs <- LSP.getVirtualFiles
  let trace x = otTracedHandler "Notification" (show _method) $ \sp -> do
        traceWithSpan sp _params
        x
  writeChan chan $ ReactorNotification (trace $ LSP.runLspT env $ k ide vfs _params)
