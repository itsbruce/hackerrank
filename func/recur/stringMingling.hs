mingle :: [a] -> [a] -> [a]
mingle xs ys = foldr (\(a1, a2) b -> a1 : a2 : b) [] $ zip xs ys

main = do
    p <- getLine
    q <- getLine
    putStrLn $ mingle p q
