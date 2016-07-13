type NString = (Integer, String)

zeroNS = (0, [])

consNS :: Char -> NString -> NString
consNS c (n, xs) = (n + 1, c : xs)

pc :: String -> String -> (NString, NString, NString)
pc = go (zeroNS, zeroNS, zeroNS)
    where
        go t@(_, _, _) [] [] = t
        go (p, l, r) (x:xs) [] = go (p, consNS x l, r) xs []
        go (p, l, r) [] (y:ys) = go (p, l, consNS y r) [] ys
        go (p, l@(0, []), r@(0, [])) (x:xs) (y:ys)
            | x == y = go (consNS x p, l, r) xs ys
            | otherwise = go (p, consNS x l, consNS y r) xs ys
        go (p, l, r) (x:xs) (y:ys) = go (p, consNS x l, consNS y r) xs ys

showNS (0, _) = "0"
showNS (n, xs) = (show n) ++ " " ++ (reverse xs)

toList (p, l, r) = [p, l, r]

main = do
    xs <- getLine
    ys <- getLine
    mapM putStrLn $ map showNS $ toList $ pc xs ys
