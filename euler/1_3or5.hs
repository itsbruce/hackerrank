import Control.Monad

sumDlessT :: Integer -> Integer -> Integer
sumDlessT t d =
    let n = div (t - 1) d
    in  div (n  * (d * 2 + (n - 1) * d)) 2

sumLessThan :: Integer -> Integer
sumLessThan t =
    let sum1 = sumDlessT t
    in  (sum1 3) + (sum1 5) - (sum1 15)

tests :: Int -> IO [Integer]
tests n = replicateM n readLn

main = do
    t <- readLn
    ts <- tests t
    let results = map sumLessThan ts
    mapM (putStrLn . show) results
