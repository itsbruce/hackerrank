main = do
    n <- readLn
    xs <- fmap ((map read) . (take n) . words) getLine
    print $ sum xs
