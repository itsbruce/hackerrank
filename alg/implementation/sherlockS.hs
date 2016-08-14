import Control.Monad (replicateM)
import Data.List (scanl1)

squares = scanl1 (+) $ iterate (+2) 1

countSquares :: Int -> Int -> Int
countSquares x y = length $ takeWhile (<= y) $ dropWhile (< x) squares

runTest = do
    [x, y] <- fmap (take 2 . map read . words) getLine
    return $ countSquares x y

main = do
    t <- readLn
    xs <- replicateM t runTest
    mapM print xs
