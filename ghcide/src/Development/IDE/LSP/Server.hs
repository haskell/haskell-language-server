{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}
module Development.IDE.LSP.Server
  ( ReactorMessage(..)
  , ReactorChan
  , ServerM
  , requestHandler
  , notificationHandler
  ) where

import           Control.Monad.Reader
import           Development.IDE.Core.Shake
import           Development.IDE.Core.Tracing
import           Ide.Types                    (HasTracing, traceWithSpan)
import           Language.LSP.Server          (Handlers, LspM)
import qualified Language.LSP.Server          as LSP
import           Language.LSP.Types
import           UnliftIO.Chan

data ReactorMessage
  = ReactorNotification (IO ())
  | ReactorRequest SomeLspId (IO ()) (ResponseError -> IO ())

type ReactorChan = Chan ReactorMessage
type ServerM c = ReaderT (ReactorChan, IdeState) (LspM c)

requestHandler
  :: forall (m :: Method FromClient Request) c. (HasTracing (MessageParams m)) =>
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c (Either ResponseError (ResponseResult m)))
  -> Handlers (ServerM c)
requestHandler m k = LSP.requestHandler m $ \RequestMessage{_method,_id,_params} resp -> do
  st@(chan,ide) <- ask
  env <- LSP.getLspEnv
  let resp' = flip runReaderT st . resp
      trace x = otTracedHandler "Request" (show _method) $ \sp -> do
        traceWithSpan sp _params
        x
  writeChan chan $ ReactorRequest (SomeLspId _id) (trace $ LSP.runLspT env $ resp' =<< k ide _params) (LSP.runLspT env . resp' . Left)

notificationHandler
  :: forall (m :: Method FromClient Notification) c. (HasTracing (MessageParams m)) =>
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c ())
  -> Handlers (ServerM c)
notificationHandler m k = LSP.notificationHandler m $ \NotificationMessage{_params,_method}-> do
  (chan,ide) <- ask
  env <- LSP.getLspEnv
  let trace x = otTracedHandler "Notification" (show _method) $ \sp -> do
        traceWithSpan sp _params
        x
  writeChan chan $ ReactorNotification (trace $ LSP.runLspT env $ k ide _params)


