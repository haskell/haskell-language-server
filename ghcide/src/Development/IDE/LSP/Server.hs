-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
module Development.IDE.LSP.Server
  ( WithMessage(..)
  , PartialHandlers(..)
  ) where


import Data.Default

import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import Development.IDE.Core.IdeConfiguration
import Development.IDE.Core.Service

data WithMessage = WithMessage
    {withResponse :: forall m req resp . (Show m, Show req) =>
        (ResponseMessage resp -> LSP.FromServerMessage) -> -- how to wrap a response
        (LSP.LspFuncs IdeConfiguration -> IdeState -> req -> IO (Either ResponseError resp)) -> -- actual work
        Maybe (LSP.Handler (RequestMessage m req resp))
    ,withNotification :: forall m req . (Show m, Show req) =>
        Maybe (LSP.Handler (NotificationMessage m req)) -> -- old notification handler
        (LSP.LspFuncs IdeConfiguration -> IdeState -> req -> IO ()) -> -- actual work
        Maybe (LSP.Handler (NotificationMessage m req))
    ,withResponseAndRequest :: forall m rm req resp newReqParams newReqBody.
        (Show m, Show rm, Show req, Show newReqParams, Show newReqBody) =>
        (ResponseMessage resp -> LSP.FromServerMessage) -> -- how to wrap a response
        (RequestMessage rm newReqParams newReqBody -> LSP.FromServerMessage) -> -- how to wrap the additional req
        (LSP.LspFuncs IdeConfiguration -> IdeState -> req -> IO (resp, Maybe (rm, newReqParams))) -> -- actual work
        Maybe (LSP.Handler (RequestMessage m req resp))
    }

newtype PartialHandlers = PartialHandlers (WithMessage -> LSP.Handlers -> IO LSP.Handlers)

instance Default PartialHandlers where
    def = PartialHandlers $ \_ x -> pure x

instance Semigroup PartialHandlers where
    PartialHandlers a <> PartialHandlers b = PartialHandlers $ \w x -> a w x >>= b w

instance Monoid PartialHandlers where
    mempty = def
