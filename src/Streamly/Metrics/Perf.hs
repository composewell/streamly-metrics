module Streamly.Metrics.Perf
    (
      benchWith
    , bench
    , benchOnWith
    )
where

import GHC.Stats (getRTSStats, getRTSStatsEnabled, RTSStats(..))
import Streamly.Internal.Data.Time.Clock
    (getTime, Clock (Monotonic, ProcessCPUTime))
import Streamly.Internal.Data.Time.Units (NanoSecond64, fromAbsTime)
import Streamly.Metrics.Channel (Channel, send)
import Streamly.Metrics.Measure (bracketWith)
import Streamly.Metrics.Perf.Type (PerfMetrics(..))
import Streamly.Metrics.Perf.RUsage (getRuMetrics, pattern RUsageSelf)
import System.Mem (performGC)

{-# INLINE getProcMetrics #-}
getProcMetrics :: IO [PerfMetrics]
getProcMetrics = do
    time <- getTime Monotonic
    cpu <- getTime ProcessCPUTime

    let cpuSec = fromIntegral (fromAbsTime cpu :: NanoSecond64) * 1e-9
    let timeSec = fromIntegral (fromAbsTime time :: NanoSecond64) * 1e-9
    return
        [ CPUTime cpuSec
        , MonotonicTime timeSec
        ]

-- Compatible with GHC 8.2 (base 4.10) onwards
{-# INLINE getGcMetrics #-}
getGcMetrics :: IO [PerfMetrics]
getGcMetrics = do
    res <- getRTSStatsEnabled
    if res
    then do
        stats <- getRTSStats
        pure
            [ GcAllocatedBytes (fromIntegral (allocated_bytes stats))
            , GcCopiedBytes (fromIntegral (copied_bytes stats))
            , GcMaxMemInUse (fromIntegral (max_mem_in_use_bytes stats))
            ]
    else pure []

{-# INLINE getPerfMetrics #-}
getPerfMetrics :: IO [PerfMetrics]
getPerfMetrics = do
    -- getGcMetrics
    procMetrics <- getProcMetrics
    gcMetrics <- getGcMetrics
    ruMetrics <- getRuMetrics RUsageSelf
    return $ concat [procMetrics, gcMetrics, ruMetrics]

{-# INLINE preRun #-}
preRun :: IO [PerfMetrics]
preRun = do
  performGC
  getPerfMetrics

{-# INLINE postRun #-}
postRun :: [PerfMetrics] -> IO [PerfMetrics]
postRun stats = do
  -- We should not leave garbage behind. Any GC work is part of the function
  -- being benchmarked. We start with no garbage before we start measuring and
  -- we leave no garbage behind. However, this also adds a constant overhead of
  -- GC which would otherwise be lesser if we do not perform GC too often.
  -- So this may show inflated cpu times but the numbers would be consistent
  -- across iterations.
  --
  -- XXX We can have this as an option.
  -- XXX The allocations and cycles consumed by the measuring functions also
  -- add to the benchmark stats. We could run a dummy function and measure the
  -- overhead and deduct that overhead. However, this is pretty small and
  -- should not matter when benchmarking large functions.
  -- performGC
  stats1 <- getPerfMetrics
  return $ zipWith (-) stats1 stats

-- | Benchmark a function application returning the function output and the
-- performance metrics.
benchWith :: (a -> IO b) -> a -> IO (b, [PerfMetrics])
benchWith = bracketWith preRun postRun

-- | Like 'benchWith' but benchmark an action instead.
bench :: IO a -> IO (a, [PerfMetrics])
bench action = benchWith (const action) ()

-- | Benchmark a function application and the send the results to the specified
-- metrics channel.
benchOnWith :: Channel PerfMetrics -> String -> (a -> IO b) -> a -> IO b
benchOnWith chan desc f arg = do
    (r, xs) <- benchWith f arg
    send chan desc xs
    return r
