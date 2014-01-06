import Control.Applicative

combos :: Int -> [Int] -> Int
combos target = matches . subSeqTotals target . inputs
    where coins = [10,5,2,1]
          inputs = map explode . zip coins . reverse
          explode (c, n) =  reverse $ takeWhile (<= target) $ map (* c) [0..n]
          matches = length . filter (== target)
          subSeqTotals _ [] = [0]
          subSeqTotals n (y:ys) = let s = subSeqTotals ys
                                in f (liftA2 (+) y s)
          maxAvailable = sum . concat . map (take 1)
          f = filter (<= target)

testCases [] = return ()
testCases [x] = error "Wrong number of arguments"
testCases (x:y:rest) = do
    putStrLn $ show $ combos (read x) $ map read $ words y
    testCases rest

main = do
    n <- readLn
    c <- getContents
    testCases $ take (n * 2) $ lines c
