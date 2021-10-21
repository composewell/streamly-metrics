module Streamly.Metrics.Perf
    (
      benchWith
    , bench
    )
where

import Streamly.Metrics.Type (Counter(..), GaugeMax(..))
import Streamly.Metrics.Measure (bracketWith)
import Data.Word (Word64)
import System.Mem (performGC)
import System.CPUTime (getCPUTime)
import GHC.Stats

data Stats =
    CpuSeconds !(Counter Double)
  | GcAllocatedBytes !(Counter Word64)
  | GcCopiedBytes !(Counter Word64)
  | GcMaxMemInUse !(GaugeMax Word64)
    deriving (Show)

#define UNARY_OP_ONE(constr,op) op (constr a) = constr (op a)
#define UNARY_OP(op) \
    UNARY_OP_ONE(CpuSeconds,op); \
    UNARY_OP_ONE(GcAllocatedBytes,op); \
    UNARY_OP_ONE(GcCopiedBytes,op); \
    UNARY_OP_ONE(GcMaxMemInUse,op);

#define INFIX_OP_ONE(constr,op) constr a op constr b = constr (a op b)
#define FUNC_OP_ONE(constr,op) constr a `op` constr b = constr (a `op` b)

#define INFIX_OP(op) \
    INFIX_OP_ONE(CpuSeconds,op); \
    INFIX_OP_ONE(GcAllocatedBytes,op); \
    INFIX_OP_ONE(GcCopiedBytes,op); \
    INFIX_OP_ONE(GcMaxMemInUse,op); \
    _ op _ = error "Cannot operate on different types of metrics";

-- XXX Can we derive this generically?
instance Num Stats where
    UNARY_OP(signum)
    UNARY_OP(abs)
    INFIX_OP(+)
    INFIX_OP(-)
    INFIX_OP(*)

-- Compatible with GHC 8.2 (base 4.10) onwards
getStats :: IO [Stats]
getStats = do
    cpuPico <- getCPUTime
    let cpuSec = fromIntegral cpuPico / (10^(12 :: Int))
    res <- getRTSStatsEnabled
    if res
    then do
        stats <- getRTSStats
        pure
            [ CpuSeconds cpuSec
            , GcAllocatedBytes (Counter (allocated_bytes stats))
            , GcCopiedBytes (Counter (copied_bytes stats))
            , GcMaxMemInUse (GaugeMax (max_mem_in_use_bytes stats))
            ]
    else pure
            [ CpuSeconds cpuSec
            , GcAllocatedBytes 0
            , GcCopiedBytes 0
            , GcMaxMemInUse 0
            ]

preRun :: IO [Stats]
preRun = do
  performGC
  getStats

postRun :: [Stats] -> IO [Stats]
postRun stats = do
  performGC
  stats1 <- getStats
  return $ zipWith (-) stats1 stats

benchWith :: (a -> IO b) -> a -> IO (b, [Stats])
benchWith = bracketWith preRun postRun

bench :: IO a -> IO (a, [Stats])
bench action = benchWith (const action) ()
