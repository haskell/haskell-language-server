{-# LANGUAGE CPP             #-}
#ifdef STM_STATS
{-# LANGUAGE RecordWildCards #-}
#endif
module Control.Concurrent.STM.Stats
    ( atomicallyNamed
    , atomically
    , getSTMStats
    , dumpSTMStats
    , module Control.Concurrent.STM
    ) where

import           Control.Concurrent.STM hiding (atomically)
import qualified Control.Concurrent.STM as STM
import           Data.Map               (Map)
#ifdef STM_STATS
import           Control.Exception      (BlockedIndefinitelyOnSTM, Exception,
                                         catch, throwIO)
import           Control.Monad
import           Data.IORef
import qualified Data.Map.Strict        as M
import           Data.Time              (getCurrentTime)
import           GHC.Conc               (unsafeIOToSTM)
import           System.IO
import           System.IO.Unsafe
import           Text.Printf
#endif

atomicallyNamed :: String -> STM a -> IO a
atomically :: STM a -> IO a
dumpSTMStats :: IO ()
getSTMStats :: IO (Map String (Int,Int))

#ifndef STM_STATS

getSTMStats = pure mempty
atomicallyNamed _ = atomically
dumpSTMStats = pure ()
atomically = STM.atomically

#else
-- adapted from the STM.Stats package

atomicallyNamed = trackNamedSTM
atomically = trackSTM

-- | Global state, seems to be unavoidable here.
globalRetryCountMap :: IORef (Map String (Int,Int))
globalRetryCountMap = unsafePerformIO (newIORef M.empty)
{-# NOINLINE globalRetryCountMap #-}


-- | For the most general transaction tracking function, 'trackSTMConf', all
-- settings can be configured using a 'TrackSTMConf' value.
data TrackSTMConf = TrackSTMConf
    { tryThreshold      :: Maybe Int
        -- ^ If the number of retries of one transaction run reaches this
        -- count, a warning is issued at runtime. If set to @Nothing@, disables the warnings completely.
    , globalThreshold   :: Maybe Int
        -- ^ If the total number of retries of one named transaction reaches
        -- this count, a warning is issued. If set to @Nothing@, disables the
        -- warnings completely.
    , extendException   :: Bool
        -- ^ If this is set, a 'BlockedIndefinitelyOnSTM' exception is replaced
        -- by a 'BlockedIndefinitelyOnNamedSTM' exception, carrying the name of
        -- the exception.
    , warnFunction      :: String -> IO ()
        -- ^ Function to call when a warning is to be emitted.
    , warnInSTMFunction :: String -> IO ()
        -- ^ Function to call when a warning is to be emitted during an STM
        -- transaction. This is possibly dangerous, see the documentation to
        -- 'unsafeIOToSTM', but can be useful to detect transactions that keep
        -- retrying forever.
    }

-- | The default settings are:
--
-- > defaultTrackSTMConf = TrackSTMConf
-- >    { tryThreshold =      Just 10
-- >    , globalThreshold =   Just 3000
-- >    , exception =         True
-- >    , warnFunction =      hPutStrLn stderr
-- >    , warnInSTMFunction = \_ -> return ()
-- >    }
defaultTrackSTMConf :: TrackSTMConf
defaultTrackSTMConf = TrackSTMConf
    { tryThreshold = Just 10
    , globalThreshold = Just 3000
    , extendException = True
    , warnFunction = hPutStrLn stderr
    , warnInSTMFunction = \_ -> return ()
    }

-- | A drop-in replacement for 'atomically'. The statistics will list this, and
-- all other unnamed transactions, as \"@_anonymous_@\" and
-- 'BlockedIndefinitelyOnSTM' exceptions will not be replaced.
-- See below for variants that give more control over the statistics and
-- generated warnings.
trackSTM :: STM a -> IO a
trackSTM = trackSTMConf defaultTrackSTMConf { extendException = False } "_anonymous_"

-- | Run 'atomically' and collect the retry statistics under the given name and using the default configuration, 'defaultTrackSTMConf'.
trackNamedSTM :: String -> STM a -> IO a
trackNamedSTM = trackSTMConf defaultTrackSTMConf

-- | Run 'atomically' and collect the retry statistics under the given name,
-- while issuing warnings when the configured thresholds are exceeded.
trackSTMConf :: TrackSTMConf -> String -> STM a -> IO a
trackSTMConf (TrackSTMConf {..}) name txm = do
    counter <- newIORef 0
    let wrappedTx =
            do  unsafeIOToSTM $ do
                    i <- atomicModifyIORef' counter incCounter
                    when (warnPred i) $
                        warnInSTMFunction $ msgPrefix ++ " reached try count of " ++ show i
                txm
    res <- if extendException
          then STM.atomically wrappedTx
              `catch` (\(_::BlockedIndefinitelyOnSTM) ->
                       throwIO (BlockedIndefinitelyOnNamedSTM name))
          else STM.atomically wrappedTx
    i <- readIORef counter
    doMB tryThreshold $ \threshold ->
       when (i > threshold) $
            warnFunction $ msgPrefix ++ " finished after " ++ show (i-1) ++ " retries"
    incGlobalRetryCount (i - 1)
    return res
  where
    doMB Nothing _  = return ()
    doMB (Just x) m = m x
    incCounter i = let j = i + 1 in (j, j)
    warnPred j = case tryThreshold of
        Nothing -> False
        Just n  -> j >= 2*n && (j >= 4 * n || j `mod` (2 * n) == 0)
    msgPrefix = "STM transaction " ++ name
    incGlobalRetryCount i = do
        (k,k') <- atomicModifyIORef' globalRetryCountMap $ \m ->
                let (oldVal, m') = M.insertLookupWithKey
                                    (\_ (a1,b1) (a2,b2) -> ((,) $! a1+a2) $! b1+b2)
                                    name
                                    (1,i)
                                    m
                in (m', let j = maybe 0 snd oldVal in (j,j+i))
        doMB globalThreshold $ \globalRetryThreshold ->
            when (k `div` globalRetryThreshold /= k' `div` globalRetryThreshold) $
                warnFunction $ msgPrefix ++ " reached global retry count of " ++ show k'

-- | If 'extendException' is set (which is the case with 'trackNamedSTM'), an
-- occurrence of 'BlockedIndefinitelyOnSTM' is replaced by
-- 'BlockedIndefinitelyOnNamedSTM', carrying the name of the transaction and
-- thus giving more helpful error messages.
newtype BlockedIndefinitelyOnNamedSTM = BlockedIndefinitelyOnNamedSTM String

instance Show BlockedIndefinitelyOnNamedSTM where
    showsPrec _ (BlockedIndefinitelyOnNamedSTM name) =
        showString $ "thread blocked indefinitely in STM transaction" ++ name

instance Exception BlockedIndefinitelyOnNamedSTM



-- | Fetches the current transaction statistics data.
--
-- The map maps transaction names to counts of transaction commits and
-- transaction retries.
getSTMStats = readIORef globalRetryCountMap

-- | Dumps the current transaction statistics data to 'System.IO.stderr'.
dumpSTMStats = do
    stats <- getSTMStats
    time <- show <$> getCurrentTime
    hPutStrLn stderr $ "STM transaction statistics (" ++ time ++ "):"
    sequence_ $
        hPrintf stderr "%-22s %10s %10s %10s\n" "Transaction" "Commits" "Retries" "Ratio" :
        [ hPrintf stderr "%-22s %10d %10d %10.2f\n" name commits retries ratio
        | (name,(commits,retries)) <- M.toList stats
        , commits > 0 -- safeguard
        , let ratio = fromIntegral retries / fromIntegral commits :: Double
        ]


#endif
