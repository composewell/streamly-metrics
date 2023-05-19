{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Aggregator
    ( translateThreadEvents
    , collectThreadCounter
    )
where

import Data.Int (Int64)
import Data.Word (Word32)
import EventParser (Event (..), Counter(..), Location(..))
import Streamly.Internal.Data.Fold (Fold(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Event processing
-------------------------------------------------------------------------------

-- XXX It would be more intuitive for scans if we use "Partial s b" instead of
-- using extract. We can avoid having to save the result in state many a times.

{-# INLINE translateThreadEvents #-}
translateThreadEvents ::
    Fold IO Event [((Word32, String, Counter), (Location, Int64))]
translateThreadEvents = Fold step initial extract

    where

    initial = pure $ Partial $ Tuple' Map.empty []

    {-
    -- OneShot logs instead of pre/post brackets
    threadEvent2 mp tid ctr1 v1 ctr2 v2 =
        pure $ Partial $ Tuple' mp
            ((if v1 /= 0
            then [((tid, "default", ctr1), (OneShot, (fromIntegral v1)))]
            else []) ++
            (if v2 /= 0
            then [((tid, "default", ctr2), (OneShot, (fromIntegral v2)))]
            else []))
            -}

    threadEventBcast mp tid value ctr loc = do
        let r = Map.lookup tid mp
        case r of
            Just set ->
                pure $ Partial $ Tuple' mp (fmap f ("default" : Set.toList set))
            Nothing ->
                pure $ Partial $ Tuple' mp [f "default"]

        where

        f x = ((tid, x, ctr), (loc, (fromIntegral value)))

    {-
    threadEvent mp tid value ctr loc =
        pure $ Partial $ Tuple' mp [f "default"]

        where

        f x = ((tid, x, ctr), (loc, (fromIntegral value)))
    -}

    windowStart mp tid tag value ctr = do
        let mp1 = Map.alter alter tid mp
        pure $ Partial $ Tuple' mp1 [f tag]

        where

        alter Nothing = Just $ Set.singleton tag
        alter (Just set) = Just $ Set.insert tag set

        f x = ((tid, x, ctr), (Start, (fromIntegral value)))

    windowEnd mp tid tag value ctr = do
        let mp1 = Map.alter alter tid mp
        pure $ Partial $ Tuple' mp1 [f tag]

        where

        alter Nothing = error "Window end when window does not exist"
        alter (Just set) = Just $ Set.delete tag set

        f x = ((tid, x, ctr), (Stop, (fromIntegral value)))

    step (Tuple' mp _) (Event tid "" counter Start value) =
        threadEventBcast mp tid value counter Start
    step (Tuple' mp _) (Event tid "" counter Stop value) =
        threadEventBcast mp tid value counter Stop

    step (Tuple' mp _) (Event tid tag counter Start value) =
        windowStart mp tid tag value counter
    step (Tuple' mp _) (Event tid tag counter Stop value) =
        windowEnd mp tid tag value counter

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
        {-
    step CollectInit (OneShot, v) =
        pure $ Partial $ CollectDone v
        -}

    -- Same handling as CollectInit
    step (CollectDone _) (Start, v)
        = pure $ Partial $ CollectPartial v
    step acc@(CollectDone _) stat@(Stop, _) = do
        putStrLn $ "Error: Stop event when counter is not initialized." ++ show stat
        pure $ Partial acc
        {-
    step (CollectDone _) (OneShot, v) =
        pure $ Partial $ CollectDone v
        -}

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
        {-
    step (CollectPartial _) (OneShot, v) = do
        putStrLn $ "Error: Bad event data, cannot be in CollectPartial state for a one shot counter."
        pure $ Partial $ CollectDone v
        -}

    extract CollectInit = pure Nothing
    extract (CollectPartial _) = pure Nothing
    extract (CollectDone v) = pure (Just v)
