module Streamly.Metrics.Perf
    (
      benchWith
    , bench
    )
where

import GHC.Stats (getRTSStats, getRTSStatsEnabled, RTSStats(..))
import Streamly.Metrics.Measure (bracketWith)
import Streamly.Metrics.Perf.Type (PerfMetrics(..))
import Streamly.Metrics.RUsage (RUsage(..), pattern RUsageSelf, getRUsage)
import Streamly.Metrics.Type (GaugeMax(..), Seconds(..), Bytes(..))
import System.CPUTime (getCPUTime)
import System.Mem (performGC)

getProcMetrics :: IO [PerfMetrics]
getProcMetrics = do
    cpuPico <- getCPUTime
    let cpuSec = fromIntegral cpuPico / (10^(12 :: Int))
    return [CPUTime cpuSec]

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

getRuMetrics :: IO [PerfMetrics]
getRuMetrics = do
    ru <- getRUsage RUsageSelf
    return
        [ RuUtime    (Seconds (ru_utime ru))
        , RuStime    (Seconds (ru_stime ru))
        , RuMaxrss   (GaugeMax (Bytes (ru_maxrss ru)))
        , RuIxrss    (GaugeMax (Bytes (ru_ixrss ru)))
        , RuIdrss    (GaugeMax (Bytes (ru_idrss ru)))
        , RuIsrss    (GaugeMax (Bytes (ru_isrss ru)))
        , RuMinflt   (ru_minflt ru)
        , RuMajflt   (ru_majflt ru)
        , RuNswap    (ru_nswap ru)
        , RuInblock  (ru_inblock ru)
        , RuOublock  (ru_oublock ru)
        , RuMsgsnd   (ru_msgsnd ru)
        , RuMsgrcv   (ru_msgrcv ru)
        , RuNsignals (ru_nsignals ru)
        , RuNvcsw    (ru_nvcsw ru)
        , RuNivcsw   (ru_nivcsw ru)
        ]

getPerfMetrics :: IO [PerfMetrics]
getPerfMetrics = do
    procMetrics <- getProcMetrics
    gcMetrics <- getGcMetrics
    ruMetrics <- getRuMetrics
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
