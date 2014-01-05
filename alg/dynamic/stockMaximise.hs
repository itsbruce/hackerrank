maxProfit :: [Int] -> Int
maxProfit xs = case xs of
        [] -> 0
        [_] -> 0
        _ -> let xmax = maximum xs
                 ys = takeWhile (< xmax) xs
                 ps = map ((-) xmax) ys
             in  (sum ps) + (maxProfit (drop (succ (length ps)) xs))
      
doTests :: [String] -> [Int]
doTests [] = []
doTests [s] = []
doTests (s1:s2:ss) = let n = read s1
                         xs = read $ "[" ++ (map (\c -> case c of
                                                  ' ' -> ','
                                                  _ -> c) s2) ++ "]"
                     in  maxProfit (take n xs) : doTests ss

main = do
    n<- readLn
    c <- getContents
    let ts = doTests $ take (n * 2) $ lines c
    putStr $ unlines $ map show ts
