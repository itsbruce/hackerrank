import Data.List (concatMap,foldl',unfoldr)

pages k n = unfoldr page 1
    where
        page x
            | x > n = Nothing
            | otherwise =
                let x' = min (n + 1) (x + k)
                in  Just ((x, x'-1), x')

specials k = foldl' special 0 . zip [1..] . concatMap (pages k)
    where
        special n (p, (x, y))
            | p >= x && p <= y = n + 1
            | otherwise = n

readN n = fmap (take n . map read . words) getLine

main = do
    [n, k] <- readN 2
    xs <- readN n
    print $ specials k xs
