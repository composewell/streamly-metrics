import Control.Concurrent (forkIO, threadDelay, yield)
import Control.Concurrent.Chan (Chan, newChan)
import Streamly.Metrics.Channel (send, printChanList)
import Streamly.Metrics.Perf (benchWith)
import Streamly.Metrics.Perf.Type (PerfMetrics)
import Streamly.Internal.Data.Time.Units (AbsTime)

import qualified Streamly.Prelude as Stream

f :: Chan (AbsTime, ([Char], [PerfMetrics])) -> IO ()
f chan = do
    (_, xs) <- benchWith (\n -> return $ sum [1..n::Int]) 1000000
    send chan ("test", xs)
    yield

main :: IO ()
main = do
    chan <- newChan
    _ <- forkIO (printChanList chan)
    Stream.drain $ Stream.replicateM 1000 (f chan)
    threadDelay 2000000
