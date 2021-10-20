{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module      : Streamly.KeyValue.Type
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
module Streamly.KeyValue.Type
    (
    -- * KeyValue
      KeyValue (..)
    )
where

import Control.Exception (assert)

-------------------------------------------------------------------------------
-- Metrics identification
-------------------------------------------------------------------------------

-- XXX Use the approach like in https://hackage.haskell.org/package/keys? Or we
-- could use a lens based approach? However, the more we abstract the less
-- comprehensible it becomes.
--
-- XXX We could also use a columnar storage (arrays) to store the keys and
-- values.  Further, keys could be stored in boxed arrays whereas the values
-- could be stored in unboxed arrays.

-- | A key value represents a sample of some value labeled with a unique key of
-- type k and having a value of type v.
--
data KeyValue k v = KeyValue !k !v

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor (KeyValue k) where
    fmap f (KeyValue k v) = KeyValue k (f v)

-- | Append two key values using the Semigroup instance of the underlying
-- value.
--
instance (Eq k, Semigroup v) => Semigroup (KeyValue k v) where
    {-# INLINE (<>) #-}
    -- XXX Only KeyValue having the same key make sense to be combined.
    -- However, matching the keys at run time would incur some cost depending
    -- on the comparison function.
    KeyValue k1 v1 <> KeyValue k2 v2 =
        assert (k1 == k2) (KeyValue k1 (v1 <> v2))

-- XXX add Num instances for folds (ZipFold?). finish the two folds, add their
-- results and create another fold to continue. Then we can make IsMetric for
-- fold. Same for ZipSerial.
--
-- XXX We should probably factor out a type class from Num that does not
-- require the fromInteger method. Then we can make more types instances of
-- that class including Metric.
--
-- Ideally we want purescript like Numeric type class hierarchy:
-- https://harry.garrood.me/numeric-hierarchy-overview/. Use a Ring rather
-- than Num. See also
-- https://pursuit.purescript.org/packages/purescript-numerics/.
--
-- Num => CommutativeRing
-- Integral => Euclidean Ring
-- Fractional => Division Ring/Field
--
-- fromInteger/toInteger should be in a separate type class or classes
-- (IsInteger/FromInteger/ToInteger).
--
-- Another approach is to use a Zippable instance and then use zipWith (+) etc
-- to zip the underlying values.

-- | Num operations on types without requiring them to have fromInteger and
-- toInteger.
class NumLike a where
    negateNum :: a -> a
    absNum :: a -> a
    signNum :: a -> a
    addNum :: a -> a -> a
    mulNum :: a -> a -> a
    subNum :: a -> a -> a

-- XXX When zipping trees of metrics we need to traverse the keys like file
-- paths or a file system tree. That means a metric should probably support a
-- lookup operation like a Map.
--
-- The simplest would be a one level Map as a metric or a metric set. Should we
-- separate these two concepts of a metric and a metric set. A metric would be
-- like a file whereas a metric set would be like a dir tree (MetricTree).
--
-- ZipList and a single level Map are a one level metric tree.
--
-- Nested metrics
instance (Eq k, NumLike v) => NumLike (KeyValue k v) where
    {-# INLINE negateNum #-}
    negateNum = fmap negateNum

    {-# INLINE absNum #-}
    absNum = fmap absNum

    {-# INLINE signNum #-}
    signNum = fmap signNum

    {-# INLINE addNum #-}
    (KeyValue k1 v1) `addNum` (KeyValue k2 v2) =
        assert (k1 == k2) (KeyValue k1 (v1 `addNum` v2))

    {-# INLINE mulNum #-}
    (KeyValue k1 v1) `mulNum` (KeyValue k2 v2) =
        assert (k1 == k2) $ KeyValue k1 (v1 `mulNum` v2)

    {-# INLINE subNum #-}
    (KeyValue k1 v1) `subNum` (KeyValue k2 v2) =
        assert (k1 == k2) $ KeyValue k1 (v1 `subNum` v2)

-- | Just a wrapper type for deriving instances
newtype KVWrapper k a = KVWrapper (KeyValue k a) deriving Functor

instance (Eq k, Num a) => NumLike (KVWrapper k a) where
    {-# INLINE negateNum #-}
    negateNum = fmap negate

    {-# INLINE absNum #-}
    absNum = fmap abs

    {-# INLINE signNum #-}
    signNum = fmap signum

    {-# INLINE addNum #-}
    (KVWrapper (KeyValue k1 v1))
        `addNum` (KVWrapper (KeyValue k2 v2)) =
            assert (k1 == k2) $ KVWrapper (KeyValue k1 (v1 + v2))

    {-# INLINE mulNum #-}
    (KVWrapper (KeyValue k1 v1))
        `mulNum` (KVWrapper (KeyValue k2 v2)) =
            assert (k1 == k2) $ KVWrapper (KeyValue k1 (v1 * v2))

    {-# INLINE subNum #-}
    (KVWrapper (KeyValue k1 v1))
        `subNum` (KVWrapper (KeyValue k2 v2)) =
            assert (k1 == k2) $ KVWrapper (KeyValue k1 (v1 - v2))

deriving via (KVWrapper k Int) instance Eq k => NumLike (KeyValue k Int)
