module Streamly.Metrics.Channel
    (
      send
    , aggregateBy
    , printChanList
    )
where

import Control.Concurrent.Chan (Chan, readChan, writeChan)
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

send :: MonadIO m => Chan (AbsTime, (k, v)) -> (k, v) -> m ()
send chan kv = do
    -- XXX should use asyncClock
    now <- liftIO $ getTime Monotonic
    liftIO $ writeChan chan (now, kv)

fromChan :: MonadAsync m => Chan a -> SerialT m a
fromChan = Stream.repeatM . (liftIO . readChan)

aggregateBy :: (MonadAsync m, Ord k, Num a) =>
    Double -> Int -> SerialT m (AbsTime, (k, a)) -> SerialT m (k, a)
aggregateBy timeout batchsize =
    Stream.classifySessionsBy 0.1 False (\x -> return $ x > 1000) timeout f

    where f = Fold.take batchsize Fold.sum

aggregateListBy :: (MonadAsync m, Ord k, Num a) =>
    Double -> Int -> SerialT m (AbsTime, (k, [a])) -> SerialT m (k, [a])
aggregateListBy timeout batchsize stream =
    fmap (second fromJust)
        $ Stream.filter (isJust . snd)
        $ Stream.classifySessionsBy
            0.1 False (return . (> 1000)) timeout f stream

    where f = Fold.take batchsize (Fold.foldl1' (zipWith (+)))

printKV :: (MonadIO m, Show k, Show a) => SerialT m (k, [a]) -> m ()
printKV =
    let f (k, xs) = liftIO $ putStrLn $ show k ++ ":\n" ++ showList xs
     in Stream.mapM_ f

printChanList :: (MonadAsync m, Show k, Ord k, Show v, Num v) =>
    Chan (AbsTime, (k, [v])) -> m ()
printChanList chan =
      fromChan chan
    & aggregateListBy 1 1
    & printKV
