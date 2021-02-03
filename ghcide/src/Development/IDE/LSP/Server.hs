{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Development.IDE.LSP.Server where

import Language.LSP.Types
import Language.LSP.Types.Lens
import Control.Lens ((^.))
import qualified Language.LSP.Server as LSP
import           Language.LSP.Server (Handlers, LspM, Handler)
import Development.IDE.Core.Shake
import UnliftIO.Chan
import Control.Monad.Reader
import Development.IDE.Core.Service
import Data.Aeson (Value)
import Development.IDE.Core.Tracing (otSetUri)
import OpenTelemetry.Eventlog (SpanInFlight, setTag)
import Data.Text.Encoding (encodeUtf8)

data ReactorMessage
  = ReactorNotification (IO ())
  | ReactorRequest SomeLspId (IO ()) (ResponseError -> IO ())

type ReactorChan = Chan ReactorMessage
type ServerM c = ReaderT (ReactorChan, IdeState) (LspM c)

requestHandler
  :: forall (m :: Method FromClient Request) c.
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c (Either ResponseError (ResponseResult m)))
  -> Handlers (ServerM c)
requestHandler m k = LSP.requestHandler m $ \RequestMessage{_id,_params} resp -> do
  st@(chan,ide) <- ask
  env <- LSP.getLspEnv
  let resp' = flip runReaderT st . resp
  writeChan chan $ ReactorRequest (SomeLspId _id) (LSP.runLspT env $ resp' =<< k ide _params) (LSP.runLspT env . resp' . Left)

notificationHandler
  :: forall (m :: Method FromClient Notification) c.
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c ())
  -> Handlers (ServerM c)
notificationHandler m k = LSP.notificationHandler m $ \NotificationMessage{_params}-> do
  (chan,ide) <- ask
  env <- LSP.getLspEnv
  writeChan chan $ ReactorNotification (LSP.runLspT env $ k ide _params)

class HasTracing a where
  traceWithSpan :: SpanInFlight -> a -> IO ()
  traceWithSpan _ _ = pure ()

instance {-# OVERLAPPABLE #-} (HasTextDocument a doc, HasUri doc Uri) => HasTracing a where
  traceWithSpan sp a = otSetUri sp (a ^. textDocument . uri)

instance HasTracing Value
instance HasTracing ExecuteCommandParams
instance HasTracing DidChangeWatchedFilesParams
instance HasTracing DidChangeWorkspaceFoldersParams
instance HasTracing DidChangeConfigurationParams
instance HasTracing InitializeParams
instance HasTracing (Maybe InitializedParams)
instance HasTracing WorkspaceSymbolParams where
  traceWithSpan sp (WorkspaceSymbolParams _ _ query) = setTag sp "query" (encodeUtf8 query)

setUriAnd ::
  (HasTextDocument params a, HasUri a Uri) =>
  (lspFuncs -> ide -> params -> IO res) ->
  lspFuncs ->
  SpanInFlight ->
  ide ->
  params ->
  IO res
setUriAnd k lf sp ide params = do
  otSetUri sp (params ^. textDocument . uri)
  k lf ide params
