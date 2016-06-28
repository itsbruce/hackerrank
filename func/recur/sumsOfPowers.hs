import Data.List

f :: Int -> Int -> Int
f x n = length . filter (== x) $ subSeqTotals $ takeWhile (<= x) $ map (^ n) [1..]
    where subSeqTotals [] = []
          subSeqTotals (y:ys) = let s = subSeqTotals ys
                                    fs = filter ((<= x) . (+ y)) s
                                in y : foldr((:) . (+ y)) s fs

main = do
    x <- readLn
    n <- readLn
    putStrLn $ show $ f x n
