module Main (main) where

import Aggregator (translateThreadEvents, collectThreadCounter, Counter (ThreadCPUTime))
import EventParser (parseLogHeader, parseDataHeader, parseEvents)
import System.Environment (getArgs)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.FileSystem.File as File

main :: IO ()
main = do
    (path:[]) <- getArgs
    let stream = File.readChunks path
    (kv, rest) <- parseLogHeader $ StreamK.fromStream stream
    -- putStrLn $ show kv
    events <- parseDataHeader rest
    Stream.fold (Fold.drainMapM print)
        $ Stream.catMaybes
        $ Stream.postscan (collectThreadCounter ThreadCPUTime)
        $ fmap snd
        $ Stream.catMaybes
        $ fmap translateThreadEvents
        $ parseEvents kv events
    return ()
