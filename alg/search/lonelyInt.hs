import qualified Data.Map as M

single :: [Int] -> Int
single = head . M.keys . M.filter (== 1) . foldr (\i m -> M.insertWith (+) i 1 m) M.empty

main = do
    n <- readLn
    s <- getLine
    let xs = map read $ take n $ words s
    putStrLn $ show $ single xs
