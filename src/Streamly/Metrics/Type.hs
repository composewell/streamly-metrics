module Streamly.Metrics.Type
    (
      Metric (..)
    , Series (..)
    , Event (..)
    , Sequenced (..)
    )
where

import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Array.Foreign (Array)
import Streamly.Internal.Data.Time.Units (AbsTime)
import Data.Word (Word64)

-- | A value tagged by an ordered value e.g. a timestamp or a sequence number.
--  XXX Just need an Ord instance?
--
data Event a = Event AbsTime a    -- XXX use a tupel and derive Ord?

-- A value tagged with a sequence number.
data Sequenced a = Sequenced Int a

-- We can either have a IsMetric type class and require the type to have a
-- "key" or we can have a Metric type with an explicit key.
--
-- | A Metric has a unique key k and a value v. The value v must have a
-- Monoid and an Ord instance.
--
data Metric k v = Metric k v

-- XXX If v is a Monoid, then Array v must have a Monoid instance.

-- Storage representation for a series of values for the same key.
data Series k v = Series k (Array v)

