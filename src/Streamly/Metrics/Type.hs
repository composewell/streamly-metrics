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
    )
where

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
