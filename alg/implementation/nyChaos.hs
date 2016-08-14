import Control.Monad (replicateM)
import Data.List (foldl')

cost (n, b)
    | b - n > 2 = Nothing
    | otherwise = Just (b - n)

carry mc mb = do
        (n, c1, c2) <- mc
        b <- mb
        go b n c1 c2
    where
        dec n = max 0 (n - 1)
        go 2 n c1 c2 = return (n + 2, c1, c2 + 1)
        go a n c1 c2
            | a == 0 - (c1 + c2) = return (n, c2, 0)
            | a > 0 - (c1 + c2) = return (n + 1, c1 + c2 + 1, 0)
            | otherwise = Nothing

costs = foldl' carry (Just (0,0,0)) . map cost . zip [1..]

readN n = fmap (take n . map read . words) getLine

runTest = do
    n <- readLn
    xs <- readN n
    return $ costs xs

showResult Nothing = "Too chaotic"
showResult (Just (x,_,_)) = show x

main = do
    t <- readLn
    results <- replicateM t runTest
    mapM putStrLn $ map showResult results
