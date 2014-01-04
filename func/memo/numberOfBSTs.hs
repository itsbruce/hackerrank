import Data.List (genericIndex)

trees :: [Integer]
trees = 1 : 1 : 2 : map f [3..]
  where f n = sum $ map (\x -> (trees !! (pred x)) * (trees !! (n - x))) [1..n]

answer = flip mod 100000007 . (!!) trees

main = do
    t <- readLn
    c <- getContents
    putStr . unlines . map show . map answer . map read . take t $ lines c
