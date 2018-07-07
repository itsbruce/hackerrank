import Control.Monad

fibs =
    let go x y = (x + y) : go y (x + y)
    in  1 : 2 : (go 1 2)

sumEvenFibs n = sum $ filter even $ takeWhile (<= n) fibs

tests :: Int -> IO [Integer]
tests n = replicateM n readLn

main = do
    t <- readLn
    ts <- tests t
    let results = map sumEvenFibs ts
    mapM (putStrLn . show) results
