import Control.Applicative ((<**>))

bsubs :: Int -> [Int] -> [[Int]]
bsubs d [] = [[]]
bsubs d (x:xs) = bsubs d xs >>= prepend x
    where
        prepend x [] = [[],[x]]
        prepend x ys@(y:_)
            | length ys < 3 && x + d == y = [ys, x : ys]
            | otherwise = [ys]

readN n = fmap (take n . map read . words) getLine

main = do
    [n, d] <- readN 2
    xs <- readN n
    print $ length $ filter ((== 3) . length) $ bsubs d xs
