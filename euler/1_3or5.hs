import Control.Monad

or35 = go [3,6..] [5,10..]
    where
        go xs@(x:xrest) ys@(y:yrest)
            | x < y = x : (go xrest ys)
            | x == y = x : (go xrest yrest)
            | otherwise = y : (go xs yrest)

sumLessThan:: Integer -> Integer
sumLessThan n = sum $ takeWhile (< n) or35

tests :: Int -> IO [Integer]
tests n = replicateM n readLn

main = do
    t <- readLn
    ts <- tests t
    let results = map sumLessThan ts
    mapM (putStrLn . show) results
