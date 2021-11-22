module Streamly.Metrics.Perf.Type
    (
      PerfMetrics (..)
    )
where

import Data.Word (Word64)
import Streamly.Metrics.Type (GaugeMax(..), Seconds(..), Bytes(..))

-- Use Counter/Gauge as the outer constructor and Bytes/Seconds as the inner
-- constuctor.
--
-- The order is important, related stats are grouped/sorted in that order for
-- presentation purposes.
data PerfMetrics =
  -- | MonotonicTime and GcElapsedTime both should provide the same figures.
    MonotonicTime !(Seconds Double)
  -- | In a single threaded system with unbound threads 'ProcessCPUTime',
  -- 'ThreadCPUTime', ('RuUtime' + 'RuStime) and 'GcCpuTime' would be the same.
  -- Note that a binary built with `-threaded` and using `-N1` RTS options is
  -- not the same as single threaded because the GC thread may still run in a
  -- separate OS thread.
  --
  -- Note that if the perf stats measurement pre and post calls straddle over a
  -- blocking Haskell thread then the stats may include cpuTime for all other
  -- threads that might have run until our post call stats collection occurs.
  -- So it may not reflect the accurate measurement of perf stats of the call
  -- in question.
  --
  | ProcessCPUTime !(Seconds Double)
  | ThreadCPUTime !(Seconds Double)

    -- GC Memory Stats
  | GcElapsedTime !(Seconds Double)
      -- | 'GcMutatorElapsedTime' and 'GcGcElapsedTime' should add up to
      -- 'GcElapsedTime'.
      | GcMutatorElapsedTime !(Seconds Double)
      | GcGcElapsedTime !(Seconds Double)

  | GcCpuTime !(Seconds Double)
      -- | 'GcMutatorCpuTime' and 'GcGcCpuTime' should add up to 'GcCpuTime'.
      | GcMutatorCpuTime !(Seconds Double)
      | GcGcCpuTime !(Seconds Double)

  | GcAllocatedBytes !(Bytes Word64)
  | GcCopiedBytes !(Bytes Word64)
  | GcMaxMemInUse !(GaugeMax (Bytes Word64))

    -- rusage Stats
    -- | 'RuUtime' and 'RuStime' should add up to 'ProcessCPUTime'.
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
  | Count       !Word64
    deriving (Show)

#define UNARY_OP_ONE(constr,op) op (constr a) = constr (op a)
#define UNARY_OP(op) \
    UNARY_OP_ONE(MonotonicTime,op); \
    UNARY_OP_ONE(ProcessCPUTime,op); \
    UNARY_OP_ONE(ThreadCPUTime,op); \
    UNARY_OP_ONE(GcAllocatedBytes,op); \
    UNARY_OP_ONE(GcCopiedBytes,op); \
    UNARY_OP_ONE(GcMaxMemInUse,op); \
    UNARY_OP_ONE(GcMutatorCpuTime,op); \
    UNARY_OP_ONE(GcMutatorElapsedTime,op); \
    UNARY_OP_ONE(GcGcCpuTime,op); \
    UNARY_OP_ONE(GcGcElapsedTime,op); \
    UNARY_OP_ONE(GcCpuTime,op); \
    UNARY_OP_ONE(GcElapsedTime,op); \
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
    UNARY_OP_ONE(RuNivcsw,op); \
    UNARY_OP_ONE(Count,op);

#define INFIX_OP_ONE(constr,op) constr a op constr b = constr (a op b)
#define FUNC_OP_ONE(constr,op) constr a `op` constr b = constr (a `op` b)

#define INFIX_OP(op) \
    INFIX_OP_ONE(MonotonicTime,op); \
    INFIX_OP_ONE(ProcessCPUTime,op); \
    INFIX_OP_ONE(ThreadCPUTime,op); \
    INFIX_OP_ONE(GcAllocatedBytes,op); \
    INFIX_OP_ONE(GcCopiedBytes,op); \
    INFIX_OP_ONE(GcMaxMemInUse,op); \
    INFIX_OP_ONE(GcMutatorCpuTime,op); \
    INFIX_OP_ONE(GcMutatorElapsedTime,op); \
    INFIX_OP_ONE(GcGcCpuTime,op); \
    INFIX_OP_ONE(GcGcElapsedTime,op); \
    INFIX_OP_ONE(GcCpuTime,op); \
    INFIX_OP_ONE(GcElapsedTime,op); \
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
    INFIX_OP_ONE(Count,op); \
    _ op _ = error "Cannot operate on different types of metrics";

-- XXX Can we derive this generically?
instance Num PerfMetrics where
    fromInteger val = Count (fromInteger val)
    UNARY_OP(signum)
    UNARY_OP(abs)
    INFIX_OP(+)
    INFIX_OP(-)
    INFIX_OP(*)

#define DIV_OP_ONE(constr) constr a / Count b = constr (a / b)
#define DIV_DOUBLE(a,b) (a / fromIntegral b)
#define DIV_SECONDS(constr) constr (Seconds a) / (Count b) \
    = constr (Seconds DIV_DOUBLE(a,b))
#define DIV_ROUND(a,b) (round (fromIntegral a / fromIntegral b :: Double))
#define DIV_BYTES(constr) constr (Bytes a) / (Count b) \
    = constr (Bytes DIV_ROUND(a,b))
#define DIV_MAX_BYTES(constr) constr (GaugeMax (Bytes a)) / (Count _) = \
    constr (GaugeMax (Bytes a))
#define DIV_COUNT(constr) constr a / Count b = constr DIV_ROUND(a,b)

instance Fractional PerfMetrics where
    DIV_SECONDS(MonotonicTime)
    DIV_SECONDS(ProcessCPUTime)
    DIV_SECONDS(ThreadCPUTime)
    DIV_SECONDS(GcMutatorCpuTime)
    DIV_SECONDS(GcMutatorElapsedTime)
    DIV_SECONDS(GcGcCpuTime)
    DIV_SECONDS(GcGcElapsedTime)
    DIV_SECONDS(GcCpuTime)
    DIV_SECONDS(GcElapsedTime)
    DIV_BYTES(GcAllocatedBytes)
    DIV_BYTES(GcCopiedBytes)
    DIV_MAX_BYTES(GcMaxMemInUse)
    DIV_SECONDS(RuUtime)
    DIV_SECONDS(RuStime)
    DIV_MAX_BYTES(RuMaxrss)
    DIV_MAX_BYTES(RuIxrss)
    DIV_MAX_BYTES(RuIdrss)
    DIV_MAX_BYTES(RuIsrss)
    DIV_COUNT(RuMinflt)
    DIV_COUNT(RuMajflt)
    DIV_COUNT(RuNswap)
    DIV_COUNT(RuInblock)
    DIV_COUNT(RuOublock)
    DIV_COUNT(RuMsgsnd)
    DIV_COUNT(RuMsgrcv)
    DIV_COUNT(RuNsignals)
    DIV_COUNT(RuNvcsw)
    DIV_COUNT(RuNivcsw)
    Count a / Count _ = Count a
    _ / _ = error "Undefined fractional operation on PerfMetrics"
