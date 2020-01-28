-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Core.Debouncer
    ( Debouncer
    , newDebouncer
    , registerEvent
    ) where

import Control.Concurrent.Extra
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Extra
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import System.Time.Extra

-- | A debouncer can be used to avoid triggering many events
-- (e.g. diagnostics) for the same key (e.g. the same file)
-- within a short timeframe. This is accomplished
-- by delaying each event for a given time. If another event
-- is registered for the same key within that timeframe,
-- only the new event will fire.
newtype Debouncer k = Debouncer (Var (HashMap k (Async ())))

-- | Create a new empty debouncer.
newDebouncer :: IO (Debouncer k)
newDebouncer = do
    m <- newVar Map.empty
    pure $ Debouncer m

-- | Register an event that will fire after the given delay if no other event
-- for the same key gets registered until then.
--
-- If there is a pending event for the same key, the pending event will be killed.
-- Events are run unmasked so it is up to the user of `registerEvent`
-- to mask if required.
registerEvent :: (Eq k, Hashable k) => Debouncer k -> Seconds -> k -> IO () -> IO ()
registerEvent (Debouncer d) delay k fire = modifyVar_ d $ \m -> mask_ $ do
    whenJust (Map.lookup k m) cancel
    a <- asyncWithUnmask $ \unmask -> unmask $ do
        sleep delay
        fire
        modifyVar_ d (pure . Map.delete k)
    pure $ Map.insert k a m
