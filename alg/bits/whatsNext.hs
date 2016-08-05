import Control.Monad
import Data.Tuple

oddArray [1] = (,) 1 [1, 1]
oddArray [x] = (,) 2 [x - 1, 1, 1]
oddArray (1 : 1 : x : rest) = (,) (-1) $ 1 : x + 1 : rest
oddArray (1 : x : rest ) = (,) 1 $ 1 : 1 : x - 1 : rest
oddArray (x : 1 : y : rest) = (,) 0 $ x - 1 : 1 : y + 1 : rest
oddArray (x : y : rest) = (,) 2 $ x - 1 : 1 : 1 : y - 1 : rest

evenArray [x, 1] = (,) 0 [x + 1, 1]
evenArray [x, y] = (,) 1 [y - 1, x + 1, 1]
evenArray (x : 1 : 1 : y : rest) =  (,) (-2) $ x + 1 : y + 1 : rest
evenArray (x : 1 : y : rest) = (,) 0 $ x + 1 : 1 : y - 1 : rest
evenArray (x : y : 1 : z : rest) = (,) (-1) $ y - 1 : x + 1 : z + 1 : rest
evenArray (x : y : z : rest) = (,) 1 $ y - 1 : x + 1 : 1 : z - 1 : rest

next :: Int -> [Integer] -> (Int, [Integer])
next n xs =
    let f = if odd n
                then oddArray
                else evenArray
    in  swap $ fmap (+ n) $ swap $ fmap reverse $ f $ reverse xs


runTest = do
    n <- readLn
    xs <- fmap (take n . map read . words) getLine
    return $ next n xs

printResults (n, xs) = do
    print n
    putStrLn $ unwords $ map show xs

main = do
    t <- readLn
    ts <- replicateM t runTest
    mapM printResults ts
