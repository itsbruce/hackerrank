import Control.Monad
import Data.Bits
import Data.Word

andProduct :: Word32 -> Word32 -> Word32
andProduct = step 0
    where
        step n x y
            | x == y = shift x n
            | otherwise = step (n + 1) (shift x (-1)) (shift y (-1))

readXY = do
    [x, y] <- fmap ((take 2) . (map read) . words) getLine
    return (x, y)

main = do
    t <- readLn
    xs <- replicateM t readXY
    let ps = map (uncurry andProduct) xs
    mapM_ print ps
