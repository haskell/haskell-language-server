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

data Handle = Handle {
      logSeriousError :: HasCallStack => T.Text -> IO ()
    , logDebug :: HasCallStack => T.Text -> IO ()
    }

makeNopHandle :: Handle
makeNopHandle = Handle e e where
    e _ = pure ()
