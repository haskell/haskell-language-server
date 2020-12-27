-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
-- | This is a compatibility module that abstracts over the
-- concrete choice of logging framework so users can plug in whatever
-- framework they want to.
module Development.IDE.Types.Logger
  ( Priority(..)
  , Logger(..)
  , logError, logWarning, logInfo, logDebug, logTelemetry
  , noLogging
  ) where

import qualified Data.Text as T


data Priority
-- Don't change the ordering of this type or you will mess up the Ord
-- instance
    = Telemetry -- ^ Events that are useful for gathering user metrics.
    | Debug -- ^ Verbose debug logging.
    | Info  -- ^ Useful information in case an error has to be understood.
    | Warning
      -- ^ These error messages should not occur in a expected usage, and
      -- should be investigated.
    | Error -- ^ Such log messages must never occur in expected usage.
    deriving (Eq, Show, Ord, Enum, Bounded)


-- | Note that this is logging actions _of the program_, not of the user.
--   You shouldn't call warning/error if the user has caused an error, only
--   if our code has gone wrong and is itself erroneous (e.g. we threw an exception).
data Logger = Logger {logPriority :: Priority -> T.Text -> IO ()}


logError :: Logger -> T.Text -> IO ()
logError x = logPriority x Error

logWarning :: Logger -> T.Text -> IO ()
logWarning x = logPriority x Warning

logInfo :: Logger -> T.Text -> IO ()
logInfo x = logPriority x Info

logDebug :: Logger -> T.Text -> IO ()
logDebug x = logPriority x Debug

logTelemetry :: Logger -> T.Text -> IO ()
logTelemetry x = logPriority x Telemetry


noLogging :: Logger
noLogging = Logger $ \_ _ -> return ()
