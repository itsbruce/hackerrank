import Control.Monad

qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort (filter (<= x) xs)
               ++ [x]
               ++ qsort (filter (> x) xs)
        
match :: Int -> [Int] -> [Int] -> String
match _ [] [] = "YES"
match k (x:xs) (y:ys) | x + y < k = "NO"
                      | otherwise = match k xs ys

main = do
    t <- readLn
    replicateM_ t $ do
        s1 <- getLine
        let (n:k:_) = map read $ words s1
        s2 <- getLine
        let xs = qsort $ map read $ take n $ words s2
        s3 <- getLine
        let ys = qsort $ map read $ take n $ words s3
        putStrLn $ match k xs $ reverse ys
