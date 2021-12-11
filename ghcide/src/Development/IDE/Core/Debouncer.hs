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
import           Control.Monad                (join, void)
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
asyncRegisterEvent
    :: (Eq k, Hashable k)
    => STM.Map k (TVar (Maybe (Seconds, IO())))
    -> Seconds
    -> k
    -> IO ()
    -> IO ()
asyncRegisterEvent d delay k fire = join $ atomicallyNamed "debouncer - register" $ do
    -- The previous TVar for this key, if any
    prev <- STM.lookup k d
    case prev of
        Just v -> do
            current <- readTVar v
            case current of
                -- Not empty, means that there is a thread running the actions
                Just _  -> writeTVar v (Just (delay, fire)) >> return (pure ())
                -- Empty = no thread. We need to start one for running the action
                Nothing -> writeTVar v (Just (delay, fire)) >> return (restart v)

        -- No previous TVar, we need to insert one and restart a thread for running the action
        Nothing
          | delay == 0 -> return fire
          | otherwise  -> do
            var <- newTVar (Just (delay, fire))
            STM.insert var k d
            return (restart var)
    where
        -- | Restart a thread to run the action stored in the given TVar
        --   Once the action is done, the thread dies.
        --   Assumes the Tvar is not empty
        restart var =
            void $ async $
                join $ atomicallyNamed "debouncer - sleep" $ do
                    contents <- readTVar var
                    case contents of
                        Nothing -> error "impossible"
                        Just (s,act) -> do
                            -- sleep for the given delay
                            -- If the TVar is written while sleeping,
                            -- the transaction will restart
                            unsafeIOToSTM $ sleep s
                            -- we are done - empty the TVar before exiting
                            writeTVar var Nothing
                            return act

-- | Debouncer used in the DAML CLI compiler that emits events immediately.
noopDebouncer :: Debouncer k
noopDebouncer = Debouncer $ \_ _ a -> a
