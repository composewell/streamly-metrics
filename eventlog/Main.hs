module Main (main) where

import Aggregator
    ( translateThreadEvents , collectThreadCounter, Counter (ThreadCPUTime)
    , CounterTagged)
import System.Environment (getArgs)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Word (Word32)
import Data.IntMap (IntMap)
import Data.Word (Word8)
import EventParser (parseLogHeader, parseDataHeader, parseEvents)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.StreamK (StreamK)
import Streamly.Internal.Data.Fold (Fold(..))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold.Container as Fold (demuxKvToMap)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.StreamK as StreamK
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

-- input (tid, counter)
-- output "Map tid x" where x is "Map (tid, window tag, counter name) ()"
{-# INLINE toStats #-}
toStats ::
    Fold
        IO
        (Word32, CounterTagged)
        (Map Word32 (Map (Word32, Maybe String, Counter) ()))
toStats = Fold.demuxKvToMap (\tid -> pure (f1 tid))

    where

    f1 k1 =
          Fold.lmap (\x -> (k1, x))
        $ Fold.scanMaybe (secondMaybe (collectThreadCounter ThreadCPUTime))
        $ Fold.lmap (\(tid, (window, x)) -> ((tid, window, ThreadCPUTime), x))
        $ Fold.demuxKvToMap (\k -> pure (f4 k))

    f4 k =
          Fold.lmap (\x -> (k, x))
        $ Fold.postscan (second stats)
        $ Fold.drainMapM print

{-# INLINE fromEvents #-}
fromEvents ::
    IntMap Int -> StreamK IO (Array Word8) -> Stream IO (Word32, CounterTagged)
fromEvents kv =
          Stream.catMaybes
        . fmap translateThreadEvents
        . parseEvents kv

main :: IO ()
main = do
    (path:[]) <- getArgs
    let stream = File.readChunks path
    (kv, rest) <- parseLogHeader $ StreamK.fromStream stream
    -- putStrLn $ show kv
    events <- parseDataHeader rest
    _ <- Stream.fold toStats (fromEvents kv events)
    return ()
