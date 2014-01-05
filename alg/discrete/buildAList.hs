import Data.List

subseqs :: [a] -> [[a]]
subseqs [] = []
subseqs (x:xs) = [x] : foldr f [] (subseqs xs)
    where f ys y = ys : (x:ys) : y

sortedseqs :: Ord a => [a] -> [[a]]
sortedseqs = sort . subseqs . sort

testCases [] = return ()
testCases (x:y:xs) = do
    putStr $ unlines $ sortedseqs $ take (read x) y
    testCases xs
    
main = do
    n <- readLn
    c <- getContents
    testCases $ take (n * 2) $ lines c
