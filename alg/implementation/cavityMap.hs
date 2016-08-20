import Control.Monad (replicateM)
import qualified Data.Map.Strict as M

cavities n m = M.mapWithKey cavity m
    where
        cavity (x,y) c
            | x == 1 = c
            | x == n = c
            | y == 1 = c
            | y == n = c
            | m M.! (x - 1, y) < c
                && m M.! (x + 1, y) < c
                    && m M.! (x, y - 1) < c
                        && m M.! (x, y + 1) < c = 'X'
            | otherwise = c

loadMatrix = M.fromList . concat . (zipWith addXY [1..])
    where
        addXY y = zip (zip (repeat y) [1..])

readMatrix n = replicateM n $ fmap (take n) getLine

unloadMatrix n = row n . M.elems
    where
        row 1 xs = [xs]
        row i xs = (take n xs) : row (i-1) (drop n xs)

main = do
    n <- readLn
    m <- fmap (loadMatrix) (readMatrix n)
    mapM putStrLn $ unloadMatrix n $ cavities n m
