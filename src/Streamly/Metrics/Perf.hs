module Streamly.Metrics.Perf
    (
      benchWith
    , bench
    )
where

import GHC.Stats (getRTSStats, getRTSStatsEnabled, RTSStats(..))
import Streamly.Internal.Data.Time.Clock
    (getTime, Clock (Monotonic, ProcessCPUTime))
import Streamly.Internal.Data.Time.Units (NanoSecond64, fromAbsTime)
import Streamly.Metrics.Measure (bracketWith)
import Streamly.Metrics.Perf.Type (PerfMetrics(..))
import Streamly.Metrics.Perf.RUsage (getRuMetrics, pattern RUsageSelf)
import System.Mem (performGC)

getProcMetrics :: IO [PerfMetrics]
getProcMetrics = do
    cpu <- getTime ProcessCPUTime
    let cpuSec = fromIntegral (fromAbsTime cpu :: NanoSecond64) * 1e-9
    time <- getTime Monotonic
    let timeSec = fromIntegral (fromAbsTime time :: NanoSecond64) * 1e-9
    return
        [ CPUTime cpuSec
        , MonotonicTime timeSec
        ]

-- Compatible with GHC 8.2 (base 4.10) onwards
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

getPerfMetrics :: IO [PerfMetrics]
getPerfMetrics = do
    procMetrics <- getProcMetrics
    gcMetrics <- getGcMetrics
    ruMetrics <- getRuMetrics RUsageSelf
    return $ procMetrics ++ gcMetrics ++ ruMetrics

preRun :: IO [PerfMetrics]
preRun = do
  performGC
  getPerfMetrics

postRun :: [PerfMetrics] -> IO [PerfMetrics]
postRun stats = do
  performGC
  stats1 <- getPerfMetrics
  return $ zipWith (-) stats1 stats

benchWith :: (a -> IO b) -> a -> IO (b, [PerfMetrics])
benchWith = bracketWith preRun postRun

bench :: IO a -> IO (a, [PerfMetrics])
bench action = benchWith (const action) ()
