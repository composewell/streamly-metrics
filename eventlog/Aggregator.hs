{-# LANGUAGE FlexibleContexts #-}
module Aggregator
    ( translateThreadEvents
    , CounterTagged
    , Counter (..)
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

-- Thread stats are special they are processed by all open windows in the
-- thread.
data CounterTagged =
      ThreadStart !Counter !Int64
    | ThreadStop !Counter !Int64
    | EndOfThread
    | WindowStart String !Counter !Int64
    | WindowStop String !Counter !Int64
    | EndOfWindow String
    deriving Show

{-# INLINE translateThreadEvents #-}
translateThreadEvents :: Event -> Maybe (Word32, CounterTagged)
translateThreadEvents (PreRunThread ts tid) =
    Just (tid, ThreadStart ThreadCPUTime (fromIntegral ts))
translateThreadEvents (PostRunThread ts tid) =
    Just (tid, ThreadStop ThreadCPUTime (fromIntegral ts))
translateThreadEvents _ = Nothing

data CollectState = CollectInit | CollectPartial Int64 | CollectDone Int64

{-# INLINE collectThreadCounter #-}
collectThreadCounter ::
    Counter -> Fold IO CounterTagged (Maybe (Maybe String, Int64))
collectThreadCounter ctr = Fold step initial extract

    where

    initial = pure $ Partial CollectInit

    step CollectInit (ThreadStart c v)
        | c == ctr = pure $ Partial $ CollectPartial v
    step CollectInit EndOfThread = pure $ Done Nothing
    step CollectInit stat@(ThreadStop _ _) = do
        putStrLn $ "Error: Stop event when counter is not initialized." ++ show stat
        pure $ Partial CollectInit
    step CollectInit _ = pure $ Partial CollectInit -- ignore other events

    -- Same handling as CollectInit
    step (CollectDone _) (ThreadStart c v)
        | c == ctr = pure $ Partial $ CollectPartial v
    step (CollectDone _) EndOfThread = pure $ Done Nothing
    step acc@(CollectDone _) stat@(ThreadStop _ _) = do
        putStrLn $ "Error: Stop event when counter is not initialized." ++ show stat
        pure $ Partial acc
    step acc@(CollectDone _) _ = pure $ Partial acc -- ignore other events

    step (CollectPartial old) (ThreadStop c new)
        | c == ctr = do
            -- putStrLn $ "new = " ++ show new ++ " old = " ++ show old
            let delta = new - old
            if delta < 0
                then error $ "counter delta is negative:"
                        ++  "new = " ++ show new ++ " old = " ++ show old
                else pure ()
            pure $ Partial $ CollectDone delta
    step (CollectPartial _) stat@(ThreadStart c v)
        | c == ctr = do
            putStrLn $ "Error: Got a duplicate thread start event " ++ show stat
            pure $ Partial $ CollectPartial v
    step (CollectPartial _) EndOfThread = do
            putStrLn $ "Error: Thread stop event without counter stop."
            pure $ Done Nothing
    step acc@(CollectPartial _) _ = pure $ Partial acc -- ignore other events

    extract CollectInit = pure Nothing
    extract (CollectPartial _) = pure Nothing
    extract (CollectDone v) = pure (Just (Nothing, v))
