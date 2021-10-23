module Streamly.Metrics.Perf
    (
      benchWith
    , bench
    )
where

import Data.Word (Word64)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, RTSStats(..))
import Streamly.Metrics.Type (GaugeMax(..), Seconds(..), Bytes(..))
import Streamly.Metrics.Measure (bracketWith)
import Streamly.Metrics.RUsage (RUsage(..), pattern RUsageSelf, getRUsage)
import System.Mem (performGC)
import System.CPUTime (getCPUTime)

-- Use Counter/Gauge as the outer constructor and Bytes/Seconds as the inner
-- constuctor.
data Stats =
    CPUTime !(Seconds Double)

    -- GC Stats
  | GcAllocatedBytes !(Bytes Word64)
  | GcCopiedBytes !(Bytes Word64)
  | GcMaxMemInUse !(GaugeMax (Bytes Word64))

    -- rusage Stats
  | RuUtime     !(Seconds Double)
  | RuStime     !(Seconds Double)
  | RuMaxrss    !(GaugeMax (Bytes Word64))
  | RuIxrss     !(GaugeMax (Bytes Word64))
  | RuIdrss     !(GaugeMax (Bytes Word64))
  | RuIsrss     !(GaugeMax (Bytes Word64))
  | RuMinflt    !Word64
  | RuMajflt    !Word64
  | RuNswap     !Word64
  | RuInblock   !Word64
  | RuOublock   !Word64
  | RuMsgsnd    !Word64
  | RuMsgrcv    !Word64
  | RuNsignals  !Word64
  | RuNvcsw     !Word64
  | RuNivcsw    !Word64
    deriving (Show)

#define UNARY_OP_ONE(constr,op) op (constr a) = constr (op a)
#define UNARY_OP(op) \
    UNARY_OP_ONE(CPUTime,op); \
    UNARY_OP_ONE(GcAllocatedBytes,op); \
    UNARY_OP_ONE(GcCopiedBytes,op); \
    UNARY_OP_ONE(RuUtime,op); \
    UNARY_OP_ONE(RuStime,op); \
    UNARY_OP_ONE(RuMaxrss,op); \
    UNARY_OP_ONE(RuIxrss,op); \
    UNARY_OP_ONE(RuIdrss,op); \
    UNARY_OP_ONE(RuIsrss,op); \
    UNARY_OP_ONE(RuMinflt,op); \
    UNARY_OP_ONE(RuMajflt,op); \
    UNARY_OP_ONE(RuNswap,op); \
    UNARY_OP_ONE(RuInblock,op); \
    UNARY_OP_ONE(RuOublock,op); \
    UNARY_OP_ONE(RuMsgsnd,op); \
    UNARY_OP_ONE(RuMsgrcv,op); \
    UNARY_OP_ONE(RuNsignals,op); \
    UNARY_OP_ONE(RuNvcsw,op); \
    UNARY_OP_ONE(RuNivcsw,op);

#define INFIX_OP_ONE(constr,op) constr a op constr b = constr (a op b)
#define FUNC_OP_ONE(constr,op) constr a `op` constr b = constr (a `op` b)

#define INFIX_OP(op) \
    INFIX_OP_ONE(CPUTime,op); \
    INFIX_OP_ONE(GcAllocatedBytes,op); \
    INFIX_OP_ONE(GcCopiedBytes,op); \
    INFIX_OP_ONE(GcMaxMemInUse,op); \
    INFIX_OP_ONE(RuUtime,op); \
    INFIX_OP_ONE(RuStime,op); \
    INFIX_OP_ONE(RuMaxrss,op); \
    INFIX_OP_ONE(RuIxrss,op); \
    INFIX_OP_ONE(RuIdrss,op); \
    INFIX_OP_ONE(RuIsrss,op); \
    INFIX_OP_ONE(RuMinflt,op); \
    INFIX_OP_ONE(RuMajflt,op); \
    INFIX_OP_ONE(RuNswap,op); \
    INFIX_OP_ONE(RuInblock,op); \
    INFIX_OP_ONE(RuOublock,op); \
    INFIX_OP_ONE(RuMsgsnd,op); \
    INFIX_OP_ONE(RuMsgrcv,op); \
    INFIX_OP_ONE(RuNsignals,op); \
    INFIX_OP_ONE(RuNvcsw,op); \
    INFIX_OP_ONE(RuNivcsw,op); \
    _ op _ = error "Cannot operate on different types of metrics";

-- XXX Can we derive this generically?
instance Num Stats where
    UNARY_OP(signum)
    UNARY_OP(abs)
    INFIX_OP(+)
    INFIX_OP(-)
    INFIX_OP(*)

getProcMetrics :: IO [Stats]
getProcMetrics = do
    cpuPico <- getCPUTime
    let cpuSec = fromIntegral cpuPico / (10^(12 :: Int))
    return [CPUTime cpuSec]

-- Compatible with GHC 8.2 (base 4.10) onwards
getGcMetrics :: IO [Stats]
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

getRuMetrics :: IO [Stats]
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

getPerfMetrics :: IO [Stats]
getPerfMetrics = do
    procMetrics <- getProcMetrics
    gcMetrics <- getGcMetrics
    ruMetrics <- getRuMetrics
    return $ procMetrics ++ gcMetrics ++ ruMetrics

preRun :: IO [Stats]
preRun = do
  performGC
  getPerfMetrics

postRun :: [Stats] -> IO [Stats]
postRun stats = do
  performGC
  stats1 <- getPerfMetrics
  return $ zipWith (-) stats1 stats

benchWith :: (a -> IO b) -> a -> IO (b, [Stats])
benchWith = bracketWith preRun postRun

bench :: IO a -> IO (a, [Stats])
bench action = benchWith (const action) ()
