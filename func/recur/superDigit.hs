type Args = (String, Integer)

superDigit :: String -> Integer
superDigit s = if sumChars < 10
                then sumChars
                else superDigit $ show sumChars
    where sumChars = sum $ map (read . (: [])) s

superN x y = superDigit $ show $ (y * superDigit x)

readArgs :: IO Args
readArgs = do
    [n, k] <- fmap words getLine
    return (n, read k)

main = do
    (n, k) <- readArgs
    print $ superN n k
