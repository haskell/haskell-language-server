-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
-- | This is a compatibility module that abstracts over the
-- concrete choice of logging framework so users can plug in whatever
-- framework they want to.
module Development.IDE.Logger
  ( Handle(..)
  , makeNopHandle
  ) where

import qualified Data.Text as T
import GHC.Stack

data Handle m = Handle {
      logError :: HasCallStack => T.Text -> m ()
    , logWarning :: HasCallStack => T.Text -> m ()
    , logInfo :: HasCallStack => T.Text -> m ()
    , logDebug :: HasCallStack => T.Text -> m ()
    }

makeNopHandle :: Monad m => Handle m
makeNopHandle = Handle e e e e where
    e _ = pure ()
