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
    | ThreadCPUTimeWall
    | ThreadUserTime
    | ThreadSystemTime
    | ThreadCtxVoluntary
    | ThreadCtxInvoluntary
    | ThreadPageFaultMinor
    | ThreadPageFaultMajor
    | ThreadIOBlockIn
    | ThreadIOBlockOut
    deriving (Show, Eq, Ord)

data Location = Start | Stop | OneShot deriving Show

-- XXX It would be more intuitive for scans if we use "Partial s b" instead of
-- using extract. We can avoid having to save the result in state many a times.

{-# INLINE translateThreadEvents #-}
translateThreadEvents ::
    Fold IO Event [((Word32, String, Counter), (Location, Int64))]
translateThreadEvents = Fold step initial extract

    where

    initial = pure $ Partial $ Tuple' Set.empty []

    threadEvent2 set tid ctr1 v1 ctr2 v2 =
        pure $ Partial $ Tuple' set
            ((if v1 /= 0
            then [((tid, "default", ctr1), (OneShot, (fromIntegral v1)))]
            else []) ++
            (if v2 /= 0
            then [((tid, "default", ctr2), (OneShot, (fromIntegral v2)))]
            else []))

    threadEvent set tid ts ctr loc =
        pure $ Partial $ Tuple' set (fmap f ("default" : Set.toList set))

        where

        f x = ((tid, x, ctr), (loc, (fromIntegral ts)))

    windowStart set tid tag ts ctr loc = do
        let set1 = Set.insert tag set
        pure $ Partial $ Tuple' set1 [f tag]

        where

        f x = ((tid, x, ctr), (loc, (fromIntegral ts)))

    windowEnd set tid tag ts ctr loc = do
        let set1 = Set.delete tag set
        pure $ Partial $ Tuple' set1 [f tag]

        where

        f x = ((tid, x, ctr), (loc, (fromIntegral ts)))

    -- CPUTime
    step (Tuple' set _) (StartThreadCPUTime tid ts) =
        threadEvent set tid ts ThreadCPUTime Start
    step (Tuple' set _) (StopThreadCPUTime tid ts) =
        threadEvent set tid ts ThreadCPUTime Stop
    step (Tuple' set _) (StartWindowCPUTime tid tag ts) =
        windowStart set tid tag ts ThreadCPUTime Start
    step (Tuple' set _) (StopWindowCPUTime tid tag ts) =
        windowEnd set tid tag ts ThreadCPUTime Start

    step (Tuple' set _) (StartThreadCPUTimeWall tid ts) =
        threadEvent set tid ts ThreadCPUTimeWall Start
    step (Tuple' set _) (StopThreadCPUTimeWall tid ts) =
        threadEvent set tid ts ThreadCPUTimeWall Stop

    -- User time
    step (Tuple' set _) (StartThreadUserTime tid ts) =
        threadEvent set tid ts ThreadUserTime Start
    step (Tuple' set _) (StopThreadUserTime tid ts) =
        threadEvent set tid ts ThreadUserTime Stop

    step (Tuple' set _) (StartThreadSystemTime tid ts) =
        threadEvent set tid ts ThreadSystemTime Start
    step (Tuple' set _) (StopThreadSystemTime tid ts) =
        threadEvent set tid ts ThreadSystemTime Stop

    step (Tuple' set _) (ThreadCtxSwitches tid vol invol) =
        threadEvent2 set tid ThreadCtxVoluntary vol ThreadCtxInvoluntary invol

    step (Tuple' set _) (ThreadPageFaults tid minor major) =
        threadEvent2 set tid ThreadPageFaultMinor minor ThreadPageFaultMajor major

    step (Tuple' set _) (ThreadIOBlocks tid ioIn ioOut) =
        threadEvent2 set tid ThreadIOBlockIn ioIn ThreadIOBlockOut ioOut

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
    step CollectInit (OneShot, v) =
        pure $ Partial $ CollectDone v

    -- Same handling as CollectInit
    step (CollectDone _) (Start, v)
        = pure $ Partial $ CollectPartial v
    step acc@(CollectDone _) stat@(Stop, _) = do
        putStrLn $ "Error: Stop event when counter is not initialized." ++ show stat
        pure $ Partial acc
    step (CollectDone _) (OneShot, v) =
        pure $ Partial $ CollectDone v

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
    step (CollectPartial _) (OneShot, v) = do
        putStrLn $ "Error: Bad event data, cannot be in CollectPartial state for a one shot counter."
        pure $ Partial $ CollectDone v

    extract CollectInit = pure Nothing
    extract (CollectPartial _) = pure Nothing
    extract (CollectDone v) = pure (Just v)
