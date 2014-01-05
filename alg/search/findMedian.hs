median :: Ord a => [a] -> a
median xs = let h = div (length xs) 2
                m = xs !! h
                middle = filter (== m) xs
                lower = filter (< m) xs
                higher = filter (> m) xs
                in if (length lower <= h) && (h < length lower + length middle)
                    then m
                    else median $ lower ++ middle ++ higher

main = do
    n <- readLn
    s <- getLine
    let xs = map read $ take n $ words s :: [Int]
    putStrLn $ show $ median xs
