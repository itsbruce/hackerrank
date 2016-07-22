import Control.Monad

main = do
    [n, k, q] <- fmap ((take 3) . (map read) . words) getLine
    xs <- fmap ((take n) . words) getLine
    let xs' = reverse $ take n $ drop k $ cycle $ reverse xs
    qs <- replicateM q readLn
    mapM putStrLn $ map (xs' !!) qs
