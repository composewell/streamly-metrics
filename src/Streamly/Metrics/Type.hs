{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module      : Streamly.Metrics.Type
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A 'Metric' represents a sample of some value labeled with a unique
-- identifier.  Metrics may be ordered with respect to each other or with
-- respect to time e.g. 'Sequence' or 'Time'.  Metrics may have different
-- semantics e.g. 'Counter' or 'Gauge'.
--
module Streamly.Metrics.Type
    (
    -- * Semantics
    -- | A counter counts how many events of a type have occurred whereas a
    -- gauge measures what is the value of a certain property at a given point
    -- of time. A counter is always @monotonic@ whereas a gauge is usually
    -- @volatile@.
      Log (..)
    , Counter (..)
    , Gauge (..)

    -- * Ordering
    -- | Metrics may be ordered with respect to each other or with respect to
    -- time.
    , Timestamped (..)
    , Sequenced (..)

    -- * Metrics
    , Metric (..)
    , IsMetric (..)
    )
where

import Control.Exception (assert)
import Control.Applicative (ZipList, liftA2)
import Streamly.Internal.Data.Array.Foreign (Array)
import Streamly.Internal.Data.Time.Units (AbsTime)

-------------------------------------------------------------------------------
-- Metric semantics
-------------------------------------------------------------------------------

-- Semantics define what operations make sense on a metric or how you can
-- combine, aggregate or collapse multiple samples of a metric.
--
-- Metrics can be aggregated in different ways:
--
-- * a -> a -> a (catenate, sum)
-- * a -> Array a -> Array a (cons)
--
-- If the type is a Monoid we can represent all interesting operations on the
-- metrici in terms of Monoid.  If "a" represents a metric then "Array a" also
-- represents a metric, if "a" is a Monoid "Array a" is also a Monoid. XXX do
-- we make the monoid instance of "Array a" append the type "a" rather than the
-- array itself like the Tee type or do we need a newtype wrapper for that?

-- | A log is the simplest metric. It has no semantics except catenation of
-- multiple logs.
--
newtype Log a = Log a

-- | Represents a monotonically increasing value like time or number of events.
--
-- Counters should support addition and subtraction operations. We can diff the
-- values of a counter at two points to figure out the change that occurred in
-- that duration.  On the other hand, maximum, minimum, range or average values
-- usually do not make sense for a counter.
--
-- It does not make sense to add different snapshots of the same counter,
-- however it makes sense to aggregate counters from multiple independent
-- sources e.g. to aggregate total number of events, or to add durations.
-- Counters could be continuous of discrete. Time is an example of a continuous
-- counter whereas instruction count is an example of a discrete counter.
--
-- Time is a fundamental counter, in fact it is a reference counter for
-- all changes, changes are measured with respect to time.

newtype Counter a = Counter a

-- | Represents the current utilization level of some resource e.g. memory
-- in use. A gauge can increase or decrease from a previous value.
--
-- It usually does not make sense to add gauge metrics collected from multiple
-- independent sources. However, maximum, minimum, range or average values make
-- sense for multiple snapshots of a gauge or the value of a gauge from
-- multiple sources.  Thus, a gauge type can be further classified into
-- maximum (e.g. peak memory) or minimum (e.g. available memory), average (e.g.
-- load average) or range.
--
newtype Gauge a = Gauge a

-------------------------------------------------------------------------------
-- Metrics ordering
-------------------------------------------------------------------------------

-- How do we distinguish (if at all) different samples of the same metric.

-- | A value tagged with a sequence number. Useful when we want to order the
-- samples of a value with respect to each other but do not care about the
-- ordering with respect to time.
--
data Sequenced a = Sequenced Int a

-- | A value tagged with a timestamp. Useful when we want to measure samples
-- with respect to time.
--
data Timestamped a = Timestamped AbsTime a    -- XXX use a tuple and derive Ord?

-------------------------------------------------------------------------------
-- Metrics identification
-------------------------------------------------------------------------------

-- How do we distinguish different metrics in a system.

-- We can either have a IsMetric type class and require the type to have a
-- "key" or we can have a Metric type with an explicit key.
--
-- | A metric represents a sample of some value labeled with a unique key of
-- type k and having a value of type v.
--
data Metric k v = Metric !k !v

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- XXX add Num instances for folds (ZipFold?). finish the two folds, add their
-- results and create another fold to continue. Then we can make IsMetric for
-- fold.
--
instance Functor (Metric k) where
    fmap f (Metric k v) = Metric k (f v)

-- | Append two metrics using the Semigroup instance of the underlying value.
--
instance (Eq k, Semigroup v) => Semigroup (Metric k v) where
    {-# INLINE (<>) #-}
    -- XXX Only Metrics having the same key make sense to be combined. However,
    -- matching the keys at run time would incur some cost depending on the
    -- comparison function.
    Metric k1 v1 <> Metric k2 v2 = assert (k1 == k2) (Metric k1 (v1 <> v2))

-- XXX We should probably factor out a type class from Num that does not
-- require the fromInteger method. Then we can make more types instances of
-- that class including Metric.
--
-- | Num operations on metrics.
class IsMetric a where
    -- fromKeyValue  -- from (key,Integer), like fromInteger
    negateMetric :: a -> a
    absMetric :: a -> a
    signumMetric :: a -> a
    addMetrics :: a -> a -> a
    multMetrics :: a -> a -> a
    subtractMetrics :: a -> a -> a

-- XXX When zipping trees of metrics we need to traverse the keys like file
-- paths or a file system tree. That means a metric should probably support a
-- lookup operation like a Map.
--
-- The simplest would be a one level Map as a metric or a metric set. Should we
-- separate these to concepts of a metric and a metric set. A metric would be
-- like a file whereas a metric set would be like a dir tree (MetricTree).
--
-- ZipList and a single level Map are a one level metric tree.
--
-- Nested metrics
instance (Eq k, IsMetric v) => IsMetric (Metric k v) where
    {-# INLINE negateMetric #-}
    negateMetric = fmap negateMetric

    {-# INLINE absMetric #-}
    absMetric = fmap absMetric

    {-# INLINE signumMetric #-}
    signumMetric = fmap signumMetric

    {-# INLINE addMetrics #-}
    (Metric k1 v1) `addMetrics` (Metric k2 v2) =
        assert (k1 == k2) (Metric k1 (v1 `addMetrics` v2))

    {-# INLINE multMetrics #-}
    (Metric k1 v1) `multMetrics` (Metric k2 v2) =
        assert (k1 == k2) $ Metric k1 (v1 `multMetrics` v2)

    {-# INLINE subtractMetrics #-}
    (Metric k1 v1) `subtractMetrics` (Metric k2 v2) =
        assert (k1 == k2) $ Metric k1 (v1 `subtractMetrics` v2)

newtype MetricWrapper k a = MetricWrapper (Metric k a) deriving Functor

instance (Eq k, Num a) => IsMetric (MetricWrapper k a) where
    {-# INLINE negateMetric #-}
    negateMetric = fmap negate

    {-# INLINE absMetric #-}
    absMetric = fmap abs

    {-# INLINE signumMetric #-}
    signumMetric = fmap signum

    {-# INLINE addMetrics #-}
    (MetricWrapper (Metric k1 v1))
        `addMetrics` (MetricWrapper (Metric k2 v2)) =
            assert (k1 == k2) $ MetricWrapper (Metric k1 (v1 + v2))

    {-# INLINE multMetrics #-}
    (MetricWrapper (Metric k1 v1))
        `multMetrics` (MetricWrapper (Metric k2 v2)) =
            assert (k1 == k2) $ MetricWrapper (Metric k1 (v1 * v2))

    {-# INLINE subtractMetrics #-}
    (MetricWrapper (Metric k1 v1))
        `subtractMetrics` (MetricWrapper (Metric k2 v2)) =
            assert (k1 == k2) $ MetricWrapper (Metric k1 (v1 - v2))

deriving via (MetricWrapper k Int) instance Eq k => IsMetric (Metric k Int)

-------------------------------------------------------------------------------
-- Orphan instances for ZipList
-------------------------------------------------------------------------------

-- Useful to Zip values inside two lists using their Monoid/Num instances.

-- | '<>' appends the corresponding elements of two ZipLists.
--
instance (Semigroup a) => Semigroup (ZipList a) where
    {-# INLINE (<>) #-}
    (<>) = liftA2 (<>)

-- | Append corresponding elements of two ZipLists using their Monoid
-- instances.
--
instance (Monoid a) => Monoid (ZipList a) where
    mempty = pure mempty
    mappend = (<>)

-- | Perform 'Num' operations on the corresponding elements of two ZipLists.
--
instance (Num a) => Num (ZipList a) where
    {-# INLINE fromInteger #-}
    fromInteger = pure . fromInteger

    {-# INLINE negate #-}
    negate = fmap negate

    {-# INLINE abs #-}
    abs = fmap abs

    {-# INLINE signum #-}
    signum = fmap signum

    {-# INLINE (+) #-}
    (+) = liftA2 (+)

    {-# INLINE (*) #-}
    (*) = liftA2 (*)

    {-# INLINE (-) #-}
    (-) = liftA2 (-)

-- | Perform 'Fractional' operations on the corresponding elements of two
-- ZipLists.
--
instance (Fractional a) => Fractional (ZipList a) where
    {-# INLINE fromRational #-}
    fromRational = pure . fromRational

    {-# INLINE recip #-}
    recip = fmap recip

    {-# INLINE (/) #-}
    (/) = liftA2 (/)

-- | Perform 'Floating' operations on the corresponding elements of two
-- ZipLists.
--
instance (Floating a) => Floating (ZipList a) where
    {-# INLINE pi #-}
    pi = pure pi

    {-# INLINE exp #-}
    exp = fmap exp

    {-# INLINE sqrt #-}
    sqrt = fmap sqrt

    {-# INLINE log #-}
    log = fmap log

    {-# INLINE sin #-}
    sin = fmap sin

    {-# INLINE tan #-}
    tan = fmap tan

    {-# INLINE cos #-}
    cos = fmap cos

    {-# INLINE asin #-}
    asin = fmap asin

    {-# INLINE atan #-}
    atan = fmap atan

    {-# INLINE acos #-}
    acos = fmap acos

    {-# INLINE sinh #-}
    sinh = fmap sinh

    {-# INLINE tanh #-}
    tanh = fmap tanh

    {-# INLINE cosh #-}
    cosh = fmap cosh

    {-# INLINE asinh #-}
    asinh = fmap asinh

    {-# INLINE atanh #-}
    atanh = fmap atanh

    {-# INLINE acosh #-}
    acosh = fmap acosh

    {-# INLINE (**) #-}
    (**) = liftA2 (**)

    {-# INLINE logBase #-}
    logBase = liftA2 logBase
