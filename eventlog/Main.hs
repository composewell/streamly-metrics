{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Aggregator
    ( translateThreadEvents , collectThreadCounter)
import System.Environment (getArgs)
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Word (Word32)
import Data.IntMap (IntMap)
import Data.Word (Word8)
import EventParser
    (parseLogHeader, parseDataHeader, parseEvents, Counter(..) , Location(..))
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.StreamK (StreamK)
import Streamly.Internal.Data.Fold (Fold(..))
import Text.Printf (printf)
import Data.Text.Format.Numbers (prettyI)
import qualified Data.List as List

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Streamly.Data.Fold as Fold
-- import qualified Streamly.Internal.Data.Fold as Fold (trace)
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

{-# INLINE toStats #-}
toStats ::
    Fold
        IO
        -- ((tid, window tag, counter), (location, value))
        ((Word32, String, Counter), (Location, Int64))
        -- Map (tid, window tag, counter) (Maybe [(stat name, value)])
        (Map (Word32, String, Counter) (Maybe [(String, Int)]))
toStats = Fold.demuxKvToMap (\k -> pure (f1 k))

    where

    f k1 collectStats =
          Fold.lmap (\x -> (k1, x))
        -- $ Fold.lmapM (\x -> print x >> pure x)
        $ Fold.scanMaybe (secondMaybe collectThreadCounter)
        $ Fold.postscan (second collectStats)
        -- $ Fold.filter (\kv -> snd (snd kv !! 0) > 50000)
        -- $ Fold.trace print
        $ Fold.lmap snd Fold.latest

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

-- Ways to present:
-- For each thread rows of counters - cols of counter stats
-- For one counter rows of threads - cols of counter stats
-- For one counter rows of threads - cols of different runs

fill :: Int -> String  -> String
fill i x =
    let len = length x
     in x ++ replicate (i - len) ' '

printTable :: [[String]] -> IO ()
printTable rows = do
    let (header:rest) = map (unwords . fillRow) rows
    putStrLn $ unlines $ header:unwords separatorRow:rest
    -- putStrLn "\n"

    where

    rowLengths = map (map length) rows -- [[Int]]
    maxLengths = List.foldl' (zipWith max) (head rowLengths) rowLengths
    separatorRow = map (\n -> replicate n '-') maxLengths
    fillRow r = zipWith (\n x -> fill n x) maxLengths r

getStatField :: String -> (k, [(String, Int)]) -> Maybe Int
getStatField x kv = List.lookup x $ snd kv

printWindowCounter ::
       [((Word32, String, Counter), [(String, Int)])]
    -> (String, Counter)
    -> IO ()
printWindowCounter statsRaw (w, ctr) = do
    if w == "default"
        then
            putStrLn $ "Entire thread stats for [" ++ show ctr ++ "]"
        else
            putStrLn
                $ "[" ++ w ++ "]" ++ " window stats for [" ++ show ctr ++ "]"
    let statsFiltered = filter select statsRaw
    let grandTotal =
            sum $ map (\x -> fromJust (getStatField "total" x)) statsFiltered
    let statsString = map (\(k, v) -> (k, map toString v)) statsFiltered
    printTable (header : map addTid statsString)
    putStrLn $ "\nGrand total: " ++ Text.unpack (prettyI (Just ',') grandTotal)
    putStrLn ""

    where

    toString (k, v) = (k, Text.unpack $ prettyI (Just ',') v)
    header = ["tid", "total", "count", "avg", "minimum", "maximum", "stddev"]
    addTid ((tid, _, _), v) = printf "%d" tid : map snd v
    select ((_, window, counter), _) = window == w && counter == ctr

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
    statsMap <- Stream.fold toStats (fromEvents kv events)
    -- statsMap :: Map (tid, window tag, counter) (Maybe [(stat name, value)])
    -- putStrLn $ ppShow r
    -- statsRaw :: [(tid, window tag, counter), [(stat name, value)]]
    let statsRaw =
            -- TODO: get the sorting field from Config/CLI
              List.sortOn (getStatField "tid")
            -- TODO: get the threshold from Config/CLI
            $ filter (\x -> fromJust (getStatField "total" x) > 0)
            $ map (\(k, v) -> (k, filter (\(k1,_) -> k1 /= "latest") v))
            $ map (\(k, v) -> (k, fromJust v))
            $ filter (\(_, v) -> isJust v)
            $ Map.toList statsMap
    let windowCounterList =
              List.nub
            $ map (\(_, window, counter) -> (window, counter))
            $ map fst statsRaw
    -- TODO: filter the counters to be printed based on Config/CLI
    -- TODO: filter the windows or threads to be printed
    -- For each (window, counter) list all threads
    mapM_ (printWindowCounter statsRaw) windowCounterList
    return ()
