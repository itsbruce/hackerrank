place :: Int -> [[Int]]
place 0 = [[]]
place n = go $ take n $ repeat [1..n] 
    where go [] = [[]]
          go (row:rest) = do
                q <- row 
                qs <- go $ safe q rest
                return (q : qs)
          safe q = notK q . notD q . notC q
          notC q = map (filter (/= q))
          notD q = (map (\(x, r) -> filter (\y -> abs(y - q) /= x) r)) . (zip [1..])
          notK q = map (\(f, r) -> filter f r) . (zip (kFilters q))
          kFilters q = (\x -> abs (x - q) /= 2) : (\x -> abs (x - q) > 1) : (repeat (const True))

solutions = length . place

main = do
    n <- readLn
    putStrLn $ show $ solutions n
