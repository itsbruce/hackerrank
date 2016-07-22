import Text.Printf

tally :: Integer -> [Integer]
tally = (1 :) . tally'
    where
        tally' x
            | x > 0 = [1,0,0]
            | x < 0 = [0,1,0]
            | otherwise = [0,0,1]

totals = sumTotals . (map tally)
    where
        sumTotals = foldr (zipWith (+)) [0,0,0,0]

percents :: [Integer] -> [Double]
percents (n : xs) = map (/ (fromIntegral n)) $ map fromIntegral xs

showP :: PrintfArg a => a -> String
showP = printf "%.6f"

main = do
    n <- readLn
    xs <- fmap ((take n) . (map read). words) getLine
    mapM putStrLn $ map showP $ percents $ totals xs
