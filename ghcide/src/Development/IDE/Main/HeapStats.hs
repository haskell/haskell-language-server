{-# LANGUAGE NumericUnderscores #-}
-- | Logging utilities for reporting heap statistics
module Development.IDE.Main.HeapStats ( withHeapStats, Log ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Word
import           Development.IDE.Types.Logger (Recorder, logWith)
import           GHC.Stats
import           Prettyprinter                (Pretty (pretty), (<+>))
import qualified Prettyprinter
import           Text.Printf                  (printf)

data Log
  = LogHeapStatsPeriod !Int
  -- logInfo l ("Logging heap statistics every "
  --             <> T.pack (printf "%.2fs" (fromIntegral @Int @Double heapStatsInterval / 1e6)))
  | LogHeapStatsDisabled
  -- logInfo l "Heap statistics are not enabled (RTS option -T is needed)"
  | LogHeapStats !Word64 !Word64
    -- format :: Word64 -> T.Text
    -- format m = T.pack (printf "%.2fMB" (fromIntegral @Word64 @Double m / 1e6))
    -- message = "Live bytes: " <> format live_bytes  <> " " <>
    --           "Heap size: " <> format heap_size
  -- logInfo l message
  deriving Show

instance Pretty Log where
  pretty log = case log of
    LogHeapStatsPeriod period ->
      "Logging heap statistics every" <+> pretty (toFormattedSeconds period)
    LogHeapStatsDisabled ->
      "Heap statistics are not enabled (RTS option -T is needed)"
    LogHeapStats liveBytes heapSize ->
      Prettyprinter.hsep
        [ "Live bytes:"
        , pretty (toFormattedMegabytes liveBytes)
        , "Heap size:"
        , pretty (toFormattedMegabytes heapSize) ]
    where
      toFormattedSeconds :: Int -> String
      toFormattedSeconds s = printf "%.2fs" (fromIntegral @Int @Double s / 1e6)

      toFormattedMegabytes :: Word64 -> String
      toFormattedMegabytes b = printf "%.2fMB" (fromIntegral @Word64 @Double b / 1e6)


-- | Interval at which to report the latest heap statistics.
heapStatsInterval :: Int
heapStatsInterval = 60_000_000 -- 60s

-- | Report the live bytes and heap size at the last major collection.
logHeapStats :: Recorder Log -> IO ()
logHeapStats l = do
  stats <- getRTSStats
  -- live_bytes is the total amount of live memory in a program
  -- (corresponding to the amount on a heap profile)
  let live_bytes = gcdetails_live_bytes (gc stats)
  -- heap_size is the total amount of memory the RTS is using
  -- this corresponds closer to OS memory usage
      heap_size  = gcdetails_mem_in_use_bytes (gc stats)
  logWith l $ LogHeapStats live_bytes heap_size

-- | An action which logs heap statistics at the 'heapStatsInterval'
heapStatsThread :: Recorder Log -> IO r
heapStatsThread l = forever $ do
  threadDelay heapStatsInterval
  logHeapStats l

-- | A helper function which lauches the 'heapStatsThread' and kills it
-- appropiately when the inner action finishes. It also checks to see
-- if `-T` is enabled.
withHeapStats :: Recorder Log -> IO r -> IO r
withHeapStats l k = do
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      logWith l $ LogHeapStatsPeriod heapStatsInterval
      withAsync (heapStatsThread l) (const k)
    else do
      logWith l LogHeapStatsDisabled
      k
