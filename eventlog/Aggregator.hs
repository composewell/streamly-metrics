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

{-# INLINE translateThreadEvents #-}
translateThreadEvents :: Event -> Maybe ((Word32, String, Counter), (Location, Int64))
translateThreadEvents (PreRunThread ts tid) =
    Just ((tid, "default", ThreadCPUTime), (Start, (fromIntegral ts)))
translateThreadEvents (PostRunThread ts tid) =
    Just ((tid, "default", ThreadCPUTime), (Stop, (fromIntegral ts)))
translateThreadEvents _ = Nothing

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
