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
        let r = Map.lookup ctr mp
        case r of
            Just set ->
                pure $ Partial $ Tuple' mp (fmap f ("default" : Set.toList set))
            Nothing ->
                pure $ Partial $ Tuple' mp [f "default"]

        where

        f tag = ((tid, tag, ctr), (loc, (fromIntegral value)))

    {-
    threadEvent mp tid value ctr loc =
        pure $ Partial $ Tuple' mp [f "default"]

        where

        f x = ((tid, x, ctr), (loc, (fromIntegral value)))
    -}

    windowStart mp tid tag value ctr = do
        let mp1 = Map.alter alter ctr mp
        pure $ Partial $ Tuple' mp1 [f tag]

        where

        alter Nothing = Just $ Set.singleton tag
        alter (Just set) =
            if Set.member tag set
                then error $ "Duplicate add " ++ "window = " ++ tag
                        ++ " tid = " ++ show tid
                else Just $ Set.insert tag set

        f x = ((tid, x, ctr), (Resume, (fromIntegral value)))

    windowEnd mp tid tag value ctr = do
        let mp1 = Map.alter alter ctr mp
        pure $ Partial $ Tuple' mp1 [f tag]

        where

        alter Nothing = error "Window end when window does not exist"
        alter (Just set) =
            if Set.member tag set
                then Just $ Set.delete tag set
                else error $ "Window end when window does not exist:"
                        ++ "window = " ++ tag ++ " tid = " ++ show tid

        f x = ((tid, x, ctr), (Exit, (fromIntegral value)))

    step (Tuple' mp _) (CounterEvent tid "" counter Resume value) =
        threadEventBcast mp tid value counter Resume
    step (Tuple' mp _) (CounterEvent tid "" counter Suspend value) =
        threadEventBcast mp tid value counter Suspend
    step _ (CounterEvent _ "" _ Exit _) = error "Unexpected Exit event"

    step (Tuple' mp _) (CounterEvent tid tag counter Resume value) =
        windowStart mp tid tag value counter
    step (Tuple' mp _) (CounterEvent tid tag counter Suspend value) =
        windowEnd mp tid tag value counter
    step _ (CounterEvent _ _ _ Exit _) = error "Unexpected Exit event"

    extract (Tuple' _ xs) = pure xs

data CollectState =
    CollectInit | CollectPartial Int64 | CollectDone Int64 | CollectExit Int64

{-# INLINE collectThreadCounter #-}
collectThreadCounter :: Fold IO (Location, Int64) (Maybe (Either Int64 Int64))
collectThreadCounter = Fold step initial extract

    where

    initial = pure $ Partial CollectInit

    step CollectInit (Resume, v) =
        pure $ Partial $ CollectPartial v
    step CollectInit stat@(Suspend, _) = do
        putStrLn $ "Error: Suspend event when counter is not initialized." ++ show stat
        pure $ Partial CollectInit
    step CollectInit stat@(Exit, _) = do
        putStrLn $ "Error: Exit event when counter is not initialized." ++ show stat
        pure $ Partial CollectInit
        {-
    step CollectInit (OneShot, v) =
        pure $ Partial $ CollectDone v
        -}

    -- Same handling as CollectInit
    step (CollectDone _) (Resume, v)
        = pure $ Partial $ CollectPartial v
    step acc@(CollectDone _) stat@(Suspend, _) = do
        putStrLn $ "Error: Suspend event when counter is not initialized." ++ show stat
        pure $ Partial acc
    step acc@(CollectDone _) stat@(Exit, _) = do
        putStrLn $ "Error: Exit event when counter is not initialized." ++ show stat
        pure $ Partial acc
        {-
    step (CollectDone _) (OneShot, v) =
        pure $ Partial $ CollectDone v
        -}

    step (CollectExit _) (Resume, v)
        = pure $ Partial $ CollectPartial v
    step acc@(CollectExit _) stat@(Suspend, _) = do
        putStrLn $ "CollectExit: Suspend event when counter is not initialized." ++ show stat
        pure $ Partial acc
    step acc@(CollectExit _) stat@(Exit, _) = do
        putStrLn $ "CollectExit: Exit event when counter is not initialized." ++ show stat
        pure $ Partial acc

    step (CollectPartial old) (Suspend, new) = do
            -- putStrLn $ "new = " ++ show new ++ " old = " ++ show old
            let delta = new - old
            if delta < 0
                then error $ "Suspend: counter delta is negative:"
                        ++  "new = " ++ show new ++ " old = " ++ show old
                else pure ()
            pure $ Partial $ CollectDone delta
    step (CollectPartial old) (Exit, new) = do
            -- putStrLn $ "new = " ++ show new ++ " old = " ++ show old
            let delta = new - old
            if delta < 0
                then error $ "Exit: counter delta is negative:"
                        ++  "new = " ++ show new ++ " old = " ++ show old
                else pure ()
            pure $ Partial $ CollectExit delta
    step (CollectPartial _) stat@(Resume, v) = do
        putStrLn $ "Error: Got a duplicate thread start event " ++ show stat
        pure $ Partial $ CollectPartial v
        {-
    step (CollectPartial _) (OneShot, v) = do
        putStrLn $ "Error: Bad event data, cannot be in CollectPartial state for a one shot counter."
        pure $ Partial $ CollectDone v
        -}

    extract CollectInit = pure Nothing
    extract (CollectPartial _) = pure Nothing
    extract (CollectDone v) = pure (Just (Right v))
    extract (CollectExit v) = pure (Just (Left v))
