{-# LANGUAGE FlexibleContexts #-}
module Aggregator
    ( translateThreadEvents
    , Counter (..)
    , Location (..)
    , collectThreadCounter
    )
where

import Data.Int (Int64)
import Data.Word (Word32)
import EventParser (Event (..))
import Streamly.Internal.Data.Fold (Fold(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Event processing
-------------------------------------------------------------------------------

-- XXX We attach a user event to a thread by looking at the previous thread
-- start event. But when there are multiple capabilities this may not be
-- possible? We need to use the thread-id on the same capability as the user
-- event. Or we can emit the tid in the user event. How does ghc-events-analyze
-- handle this? or the user event can log the thread-id as part of the tag.

data Counter =
    ThreadCPUTime
    deriving (Show, Eq, Ord)

data Location = Start | Stop deriving Show

-- XXX It would be more intuitive for scans if we use "Partial s b" instead of
-- using extract. We can avoid having to save the result in state many a times.

{-# INLINE translateThreadEvents #-}
translateThreadEvents ::
    Fold IO Event [((Word32, String, Counter), (Location, Int64))]
translateThreadEvents = Fold step initial extract

    where

    initial = pure $ Partial $ Tuple' Set.empty []

    step (Tuple' set _) (PreRunThread ts tid) =
        pure $ Partial $ Tuple' set (fmap f ("default" : Set.toList set))

        where

        f x = ((tid, x, ThreadCPUTime), (Start, (fromIntegral ts)))
    step (Tuple' set _) (PostRunThread ts tid) =
        pure $ Partial $ Tuple' set (fmap f ("default" : Set.toList set))

        where

        f x = ((tid, x, ThreadCPUTime), (Stop, (fromIntegral ts)))
    step (Tuple' set _) (PreUserCPUTime tag ts tid) = do
        let set1 = Set.insert tag set
        pure $ Partial $ Tuple' set1 [f tag]

        where

        f x = ((tid, x, ThreadCPUTime), (Start, (fromIntegral ts)))
    step (Tuple' set _) (PostUserCPUTime tag ts tid) = do
        let set1 = Set.delete tag set
        pure $ Partial $ Tuple' set1 [f tag]

        where

        f x = ((tid, x, ThreadCPUTime), (Stop, (fromIntegral ts)))
    step (Tuple' set _) (Unknown _ _) =
        pure $ Partial $ Tuple' set []

    extract (Tuple' _ xs) = pure xs

data CollectState = CollectInit | CollectPartial Int64 | CollectDone Int64

{-# INLINE collectThreadCounter #-}
collectThreadCounter :: Fold IO (Location, Int64) (Maybe Int64)
collectThreadCounter = Fold step initial extract

    where

    initial = pure $ Partial CollectInit

    step CollectInit (Start, v) =
        pure $ Partial $ CollectPartial v
    step CollectInit stat@(Stop, _) = do
        putStrLn $ "Error: Stop event when counter is not initialized." ++ show stat
        pure $ Partial CollectInit

    -- Same handling as CollectInit
    step (CollectDone _) (Start, v)
        = pure $ Partial $ CollectPartial v
    step acc@(CollectDone _) stat@(Stop, _) = do
        putStrLn $ "Error: Stop event when counter is not initialized." ++ show stat
        pure $ Partial acc

    step (CollectPartial old) (Stop, new) = do
            -- putStrLn $ "new = " ++ show new ++ " old = " ++ show old
            let delta = new - old
            if delta < 0
                then error $ "counter delta is negative:"
                        ++  "new = " ++ show new ++ " old = " ++ show old
                else pure ()
            pure $ Partial $ CollectDone delta
    step (CollectPartial _) stat@(Start, v) = do
            putStrLn $ "Error: Got a duplicate thread start event " ++ show stat
            pure $ Partial $ CollectPartial v

    extract CollectInit = pure Nothing
    extract (CollectPartial _) = pure Nothing
    extract (CollectDone v) = pure (Just v)
