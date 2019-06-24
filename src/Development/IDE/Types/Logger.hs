-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
-- | This is a compatibility module that abstracts over the
-- concrete choice of logging framework so users can plug in whatever
-- framework they want to.
module Development.IDE.Types.Logger
  ( Logger(..)
  , makeOneLogger
  , makeNopLogger
  ) where

import qualified Data.Text as T
import GHC.Stack

data Logger = Logger {
      logSeriousError :: HasCallStack => T.Text -> IO ()
    , logInfo :: HasCallStack => T.Text -> IO ()
    , logDebug :: HasCallStack => T.Text -> IO ()
    , logWarning :: HasCallStack => T.Text -> IO ()
    }

makeNopLogger :: Logger
makeNopLogger = makeOneLogger $ const $ pure ()

makeOneLogger :: (HasCallStack => T.Text -> IO ()) -> Logger
makeOneLogger x = Logger x x x x
