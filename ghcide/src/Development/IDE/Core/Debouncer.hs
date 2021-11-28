-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Core.Debouncer
    ( Debouncer
    , registerEvent
    , newAsyncDebouncer
    , noopDebouncer
    ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stats (atomicallyNamed)
import           Control.Exception
import           Control.Monad                (join)
import           Data.Hashable
import           GHC.Conc                     (unsafeIOToSTM)
import qualified StmContainers.Map            as STM
import           System.Time.Extra

-- | A debouncer can be used to avoid triggering many events
-- (e.g. diagnostics) for the same key (e.g. the same file)
-- within a short timeframe. This is accomplished
-- by delaying each event for a given time. If another event
-- is registered for the same key within that timeframe,
-- only the new event will fire.
--
-- We abstract over the debouncer used so we an use a proper debouncer in the IDE but disable
-- debouncing in the DAML CLI compiler.
newtype Debouncer k = Debouncer { registerEvent :: Seconds -> k -> IO () -> IO () }

-- | Debouncer used in the IDE that delays events as expected.
newAsyncDebouncer :: (Eq k, Hashable k) => IO (Debouncer k)
newAsyncDebouncer = Debouncer . asyncRegisterEvent <$> STM.newIO

-- | Register an event that will fire after the given delay if no other event
-- for the same key gets registered until then.
--
-- If there is a pending event for the same key, the pending event will be killed.
-- Events are run unmasked so it is up to the user of `registerEvent`
-- to mask if required.
asyncRegisterEvent :: (Eq k, Hashable k) => STM.Map k (TVar (Seconds, IO())) -> Seconds -> k -> IO () -> IO ()
asyncRegisterEvent d delay k fire = mask_ $ do
    prev <- atomically $ STM.lookup k d
    case prev of
        Just v -> do
            atomicallyNamed "debouncer - reset" $ writeTVar v (delay, fire)
        Nothing -> do
            var <- newTVarIO (delay, fire)
            _ <- asyncWithUnmask $ \unmask -> unmask $ do
                join $ atomicallyNamed "debouncer - sleep" $ do
                    (s,act) <- readTVar var
                    unsafeIOToSTM $ sleep s
                    STM.delete k d
                    return act
            atomicallyNamed "debouncer2" $ STM.insert var k d

-- | Debouncer used in the DAML CLI compiler that emits events immediately.
noopDebouncer :: Debouncer k
noopDebouncer = Debouncer $ \_ _ a -> a
