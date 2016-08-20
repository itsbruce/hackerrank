hops :: [Int] -> Int
hops = hop 0 . drop 1
    where
        hop n [] = n
        hop n (_:0:hs) = hop (n + 1) hs
        hop n (0:hs) = hop (n + 1) hs

main = do
    n <- readLn
    xs <- fmap (take n . map read . words) getLine
    print $ hops xs
