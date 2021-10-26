module Streamly.Metrics.Channel
    (
      Channel
    , newChannel
    , send
    , printChannel
    , forkChannelPrinter
    )
where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
    (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Maybe (fromJust, isJust)
import Streamly.Internal.Data.Time.Clock (getTime, Clock (Monotonic))
import Streamly.Internal.Data.Time.Units (AbsTime)
import Streamly.Metrics.Type (showList)
import Streamly.Prelude (SerialT, MonadAsync)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Prelude hiding (showList)

-------------------------------------------------------------------------------
-- Event processing
-------------------------------------------------------------------------------

-- | A metrics channel.
newtype Channel a = Channel (TBQueue (AbsTime, ([Char], [a])))

-- | Create a new metrics channel.
newChannel :: IO (Channel a)
newChannel = atomically $ do
    tbq <- newTBQueue 1
    return $ Channel tbq

-- | Send a list of metrics to a metrics channel.
-- @send channel description metrics@
send :: MonadIO m => Channel a -> String -> [a] -> m ()
send (Channel chan) desc metrics = do
    -- XXX should use asyncClock
    now <- liftIO $ getTime Monotonic
    liftIO $ atomically $ writeTBQueue chan (now, (desc, metrics))

fromChan :: MonadAsync m => TBQueue a -> SerialT m a
fromChan = Stream.repeatM . (liftIO . atomically . readTBQueue)

aggregateListBy :: (MonadAsync m, Ord k, Num a) =>
    Double -> Int -> SerialT m (AbsTime, (k, [a])) -> SerialT m (k, [a])
aggregateListBy timeout batchsize stream =
    fmap (second fromJust)
        $ Stream.filter (isJust . snd)
        $ Stream.classifySessionsBy
            0.1 False (return . (> 1000)) timeout f stream

    where f = Fold.take batchsize (Fold.foldl1' (zipWith (+)))

printKV :: (MonadIO m, Show k, Show a) => SerialT m (k, [a]) -> m b
printKV stream =
    let f (k, xs) = liftIO $ putStrLn $ show k ++ ":\n" ++ showList xs
     in Stream.mapM_ f stream >> error "printChannel: Metrics channel closed"

-- XXX Print actual batch size and also scale the results per event.

-- | Forever print the metrics on a channel to the console periodically after
-- aggregating the metrics collected till now.
printChannel :: (MonadAsync m, Show a, Num a) =>
    Channel a -> Double -> Int -> m b
printChannel (Channel chan) timeout batchSize =
      fromChan chan
    & aggregateListBy timeout batchSize
    & printKV

forkChannelPrinter :: (MonadAsync m, Show a, Num a) =>
    Channel a -> Double -> Int -> m ThreadId
forkChannelPrinter chan timeout = liftIO . forkIO . printChannel chan timeout
