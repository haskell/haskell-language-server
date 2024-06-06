{-# LANGUAGE UndecidableInstances #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.LSP.Server
  ( ReactorMessage(..)
  , ReactorChan
  , ServerM(..)
  , requestHandler
  , notificationHandler
  ) where
import           Control.Monad.IO.Unlift       (MonadUnliftIO)
import           Control.Monad.Reader
import           Development.IDE.Core.Shake
import           Development.IDE.Core.Tracing
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Server           (Handlers, LspM)
import qualified Language.LSP.Server           as LSP
import           Language.LSP.VFS
import           UnliftIO.Chan

data ReactorMessage
  = ReactorNotification (IO ())
  | forall m . ReactorRequest (LspId m) (IO ()) (TResponseError m -> IO ())

type ReactorChan = Chan ReactorMessage
newtype ServerM c a = ServerM { unServerM :: ReaderT (ReactorChan, IdeState) (LspM c) a }
  deriving (Functor, Applicative, Monad, MonadReader (ReactorChan, IdeState), MonadIO, MonadUnliftIO, LSP.MonadLsp c)

requestHandler
  :: forall m c. PluginMethod Request m =>
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c (Either (TResponseError m) (MessageResult m)))
  -> Handlers (ServerM c)
requestHandler m k = LSP.requestHandler m $ \TRequestMessage{_method,_id,_params} resp -> do
  st@(chan,ide) <- ask
  env <- LSP.getLspEnv
  let resp' :: Either (TResponseError m) (MessageResult m) -> LspM c ()
      resp' = flip (runReaderT . unServerM) st . resp
      trace x = otTracedHandler "Request" (show _method) $ \sp -> do
        traceWithSpan sp _params
        x
  writeChan chan $ ReactorRequest (_id) (trace $ LSP.runLspT env $ resp' =<< k ide _params) (LSP.runLspT env . resp' . Left)

notificationHandler
  :: forall m c. PluginMethod Notification m =>
     SMethod m
  -> (IdeState -> VFS -> MessageParams m -> LspM c ())
  -> Handlers (ServerM c)
notificationHandler m k = LSP.notificationHandler m $ \TNotificationMessage{_params,_method}-> do
  (chan,ide) <- ask
  env <- LSP.getLspEnv
  -- Take a snapshot of the VFS state on every notification
  -- We only need to do this here because the VFS state is only updated
  -- on notifications
  vfs <- LSP.getVirtualFiles
  let trace x = otTracedHandler "Notification" (show _method) $ \sp -> do
        traceWithSpan sp _params
        x
  writeChan chan $ ReactorNotification (trace $ LSP.runLspT env $ k ide vfs _params)
