import Text.Printf
e10 :: Double-> Double
e10 n = foldr (+) 0 $ scanl (\a b -> (a * n) / b) 1 [1..9]
        

main = do
    s <- getContents
    let xs = lines s
    let n = read $ head xs
    let ys = map read $ take n $ tail xs
    putStr $ unlines $ map (printf "%.4f" . e10) ys
