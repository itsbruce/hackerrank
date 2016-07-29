import Control.Monad
import Data.Bits
import Data.List (foldl')

makeSlice l r =
    let blockN n = div n 4
        makeBlock n = [n * 4, 1, (n * 4) + 3, 0]
        lBlock = drop (mod l 4) $ makeBlock $ blockN l
        rBlock = take (1 + (mod r 4)) $ makeBlock $ blockN r
        blockDiff = (blockN r) - (blockN l)
        twos = replicate (1 + (mod blockDiff 2)) 2
        slice x y
            | blockN x == blockN y = take (1 + y - x) lBlock
            | blockN x == (blockN y) - 1 = lBlock ++ rBlock
            | otherwise = lBlock ++ twos ++ rBlock
    in  slice l r

question :: Int -> Int -> Int
question l r = foldl' xor 0 $ makeSlice l r
            
readQs :: IO (Int, Int)
readQs = do
    [x, y] <- fmap ((take 2) . (map read) . words) getLine
    return (x, y)

main = do
    q <- readLn
    qs <- replicateM q readQs 
    mapM_ print $ map (uncurry question) qs
