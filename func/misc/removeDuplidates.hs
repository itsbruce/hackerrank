
uniq [] = []
uniq (x:xs) = x : (filter (/= x) (uniq xs))

main = putStrLn =<< (fmap uniq getLine)
