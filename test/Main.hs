import Control.Concurrent(threadDelay)
import Streamly.Metrics.Channel (Channel, newChannel, forkChannelPrinter)
-- import Streamly.Metrics.Channel (printChannel)
import Streamly.Metrics.Perf (benchOnWith)
import Streamly.Metrics.Perf.Type (PerfMetrics)

import qualified Streamly.Prelude as Stream
-- import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Prelude hiding (sum)

noop :: Channel PerfMetrics -> IO ()
noop chan = do
    benchOnWith chan "noop" (const (return ())) (1000000 :: Int)

sum :: Channel PerfMetrics -> IO ()
sum chan = do
    _ <- benchOnWith
        chan "sum" (Stream.sum . Stream.enumerateFromTo (1::Int)) 1000000
    return ()

main :: IO ()
main = do
    chan <- newChannel
    _ <- forkChannelPrinter chan 10 100
    Stream.drain (Stream.replicateM 1000 (noop chan))
    Stream.drain (Stream.replicateM 1000 (sum chan))
    threadDelay 1000000
    {-
    Stream.drain
        ((Stream.replicateM 1000 (noop chan) <> Stream.replicateM 1000 (sum chan))
            `Stream.parallelFst` Stream.fromEffect (printChannel chan 1 10))
    -}
