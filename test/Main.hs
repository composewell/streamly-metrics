import Streamly.Metrics.Perf
import Streamly.Metrics.Type
import Prelude hiding (showList)

main :: IO ()
main = do
    (_, xs) <- benchWith (\n -> return $ sum [1..n::Int]) 1000000
    putStrLn $ showList xs
