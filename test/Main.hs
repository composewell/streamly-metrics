import Streamly.Metrics.Perf

main :: IO ()
main = do
    r <- benchWith (\n -> return $ sum [1..n::Int]) 1000000
    print r
