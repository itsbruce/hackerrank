import Control.Monad (replicateM)
import Data.List (foldl1')
import qualified Data.Vector as V

readData n = replicateM n readRow
    where
        readRow = fmap ((take n) . (map read) . words) getLine

toVectors :: [[Integer]] -> [V.Vector Integer]
toVectors = map V.fromList

diagonalDiff n =
    let indices = take n $ zip [n-1,n-2..] [0..]
        ixs = zipWith collect indices
        collect (i, i2) v = (v V.! i, v V.! i2)
        psum (x, y) (a, b) = (x + a, y + b)
        pdiff (a, b) = b - a
    in  abs . pdiff . (foldl1' psum) . ixs

main = do
    n <- readLn
    xs <- fmap toVectors (readData n)
    print $ diagonalDiff n xs
    
