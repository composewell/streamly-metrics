module Streamly.Metrics.Perf.Type
    (
      PerfMetrics (..)
    )
where

import Data.Word (Word64)
import Streamly.Metrics.Type (GaugeMax(..), Seconds(..), Bytes(..))

-- Use Counter/Gauge as the outer constructor and Bytes/Seconds as the inner
-- constuctor.
data PerfMetrics =
    CPUTime !(Seconds Double)
  | MonotonicTime !(Seconds Double)

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
    UNARY_OP_ONE(MonotonicTime,op); \
    UNARY_OP_ONE(GcAllocatedBytes,op); \
    UNARY_OP_ONE(GcCopiedBytes,op); \
    UNARY_OP_ONE(GcMaxMemInUse,op); \
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
    INFIX_OP_ONE(MonotonicTime,op); \
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
instance Num PerfMetrics where
    UNARY_OP(signum)
    UNARY_OP(abs)
    INFIX_OP(+)
    INFIX_OP(-)
    INFIX_OP(*)
