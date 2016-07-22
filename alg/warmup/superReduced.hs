reduce :: Eq a => [a] -> [a]
reduce [] = []
reduce [x] = [x]
reduce xs@[x, y]
    | x == y = []
    | otherwise = xs
reduce (x : y : rest) = 
    let rest' = reduce (y : rest)
        (xs, ys) = splitAt 1 rest'
    in  reduce (x : xs) ++ ys

showR [] = "Empty String"
showR xs = xs

main = putStrLn =<< fmap (showR . reduce) getLine

