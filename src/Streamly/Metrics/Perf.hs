module Streamly.Metrics.Perf
    (
      benchWith
    , bench
    , benchOnWith
    , preRun
    , postRun
    )
where

import GHC.Stats (getRTSStats, getRTSStatsEnabled, RTSStats(..))
import Streamly.Internal.Data.Time.Units (NanoSecond64, fromAbsTime)
import Streamly.Metrics.Channel (Channel, send)
import Streamly.Metrics.Measure (measureWith)
import Streamly.Metrics.Perf.Type (PerfMetrics(..))
import Streamly.Metrics.Perf.RUsage (getRuMetrics, pattern RUsageSelf)
import System.Mem (performGC)

import qualified Streamly.Internal.Data.Time.Clock as Clock

{-# INLINE getProcMetrics #-}
getProcMetrics :: IO [PerfMetrics]
getProcMetrics = do
    time <- Clock.getTime Clock.Monotonic
    tcpu <- Clock.getTime Clock.ThreadCPUTime
    pcpu <- Clock.getTime Clock.ProcessCPUTime

    let tcpuSec = fromIntegral (fromAbsTime tcpu :: NanoSecond64) * 1e-9
    let pcpuSec = fromIntegral (fromAbsTime pcpu :: NanoSecond64) * 1e-9
    let timeSec = fromIntegral (fromAbsTime time :: NanoSecond64) * 1e-9
    return
        [ MonotonicTime timeSec
        , ProcessCPUTime pcpuSec
        , ThreadCPUTime tcpuSec
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
            , GcMutatorCpuTime (fromIntegral (mutator_cpu_ns stats) / 1e9)
            , GcMutatorElapsedTime
                (fromIntegral (mutator_elapsed_ns stats) / 1e9)
            , GcGcCpuTime (fromIntegral (gc_cpu_ns stats) / 1e9)
            , GcGcElapsedTime (fromIntegral (gc_elapsed_ns stats) / 1e9)
            , GcCpuTime (fromIntegral (cpu_ns stats) / 1e9)
            , GcElapsedTime (fromIntegral (elapsed_ns stats) / 1e9)
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
  -- XXX If we have nested perf measurement calls then it is a bad idea to
  -- perfromGC
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
benchWith = measureWith preRun postRun

-- | Like 'benchWith' but benchmark an action instead.
bench :: IO a -> IO (a, [PerfMetrics])
bench action = benchWith (const action) ()

-- | Benchmark a function application and the send the results to the specified
-- metrics channel.
benchOnWith :: Channel PerfMetrics -> String -> (a -> IO b) -> a -> IO b
benchOnWith chan desc f arg = do
    (r, xs) <- benchWith f arg
    send chan desc (Count 1 : xs)
    return r
