module Main (main) where

import Aggregator
    ( translateThreadEvents , collectThreadCounter)
import System.Environment (getArgs)
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Word (Word32)
import Data.IntMap (IntMap)
import Data.Word (Word8)
import EventParser
    (parseLogHeader, parseDataHeader, parseEvents, Counter(..) , Location(..))
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.StreamK (StreamK)
import Streamly.Internal.Data.Fold (Fold(..))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold.Container as Fold (demuxKvToMap)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.FileSystem.File as File

-------------------------------------------------------------------------------
-- Utility functions, can go in streamly-core
-------------------------------------------------------------------------------

{-# INLINE second #-}
second :: (Monad m, Eq a) => Fold m b c -> Fold m (a,b) (a,c)
second f = Fold.unzip (fmap fromJust Fold.the) f

{-# INLINE secondMaybe #-}
secondMaybe :: (Monad m, Eq a) =>
    Fold m b (Maybe c) -> Fold m (a,b) (Maybe (a,c))
secondMaybe f = fmap f1 (Fold.unzip (fmap fromJust Fold.the) f)

    where

    f1 (_, Nothing) = Nothing
    f1 (a, Just c) = Just (a, c)

-------------------------------------------------------------------------------
-- Application
-------------------------------------------------------------------------------

double :: Int -> Double
double = fromIntegral

untilLeft :: Monad m => Fold m b1 b2 -> Fold m (Either b1 b1) b2
untilLeft f = Fold.takeEndBy isLeft (Fold.lmap (either id id) f)

-- Statistics collection for each counter
{-# INLINE stats #-}
stats :: Fold IO Int64 [(String, Int)]
stats =
      Fold.lmap (fromIntegral :: Int64 -> Int)
    $ Fold.distribute
        [ fmap (\x -> ("latest", fromJust x)) Fold.latest
        , fmap (\x -> ("total", x)) Fold.sum
        , fmap (\x -> ("count", x)) Fold.length
        , fmap (\x -> ("avg", round x)) (Fold.lmap double Fold.mean)
        , fmap (\x -> ("minimum", fromJust x)) Fold.minimum
        , fmap (\x -> ("maximum", fromJust x)) Fold.maximum
        , fmap (\x -> ("stddev", round x)) (Fold.lmap double Fold.stdDev)
        ]

{-# INLINE threadStats #-}
threadStats :: Fold IO (Either Int64 Int64) [(String, Int)]
threadStats = untilLeft stats

{-# INLINE windowStats #-}
windowStats :: Fold IO (Either Int64 Int64) [(String, Int)]
windowStats = Fold.many (untilLeft Fold.sum) stats

-- input (tid, counter)
-- output "Map tid x" where x is "Map (tid, window tag, counter name) ()"
{-# INLINE toStats #-}
toStats ::
    Fold
        IO
        ((Word32, String, Counter), (Location, Int64))
        (Map (Word32, String, Counter) ())
toStats = Fold.demuxKvToMap (\k -> pure (f1 k))

    where

    f k1 st =
          Fold.lmap (\x -> (k1, x))
        -- $ Fold.lmapM (\x -> print x >> pure x)
        $ Fold.scanMaybe (secondMaybe collectThreadCounter)
        $ Fold.postscan (second (Fold.lmap Right st))
        -- $ Fold.filter (\kv -> snd (snd kv !! 0) > 50000)
        $ Fold.drainMapM print

    -- For the main thread
    f1 k1@(_, "default", _) = f k1 threadStats
    -- For windows inside the thread
    f1 k1@(_, _, _) = f k1 windowStats

{-# INLINE fromEvents #-}
fromEvents ::
       IntMap Int
    -> StreamK IO (Array Word8)
    -> Stream IO ((Word32, String, Counter), (Location, Int64))
fromEvents kv =
          Stream.unfoldMany Unfold.fromList
        . Stream.postscan translateThreadEvents
        -- . Stream.trace print
        . parseEvents kv

-- XXX Are the events for a particular thread guaranteed to come in order. What
-- if a thread logged events to a particular capability buffer and then got
-- scheduled on another capability before its eventlog could be flushed from
-- the previous capability?
main :: IO ()
main = do
    (path:[]) <- getArgs
    let stream = File.readChunks path
    (kv, rest) <- parseLogHeader $ StreamK.fromStream stream
    -- putStrLn $ show kv
    events <- parseDataHeader rest
    _ <- Stream.fold toStats (fromEvents kv events)
    return ()
