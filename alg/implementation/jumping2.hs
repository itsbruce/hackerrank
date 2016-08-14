import Data.List (cycle)

costs = (+ 1) . (* 2)

everyK k [] = []
everyK k xs =
    let (first, rest) = splitAt k xs
        x = drop (k - 1) first
    in x ++ everyK k rest

jumpsCost :: Int -> Int -> [Int] -> Int
jumpsCost k n xs =
    let clouds = take n $ drop 1 $ cycle xs
    in  sum $ map costs $ everyK k clouds

readN n = fmap (take n . map read . words) getLine

main = do
    [n, k] <- readN 2
    xs <- readN n
    print $ 100 - jumpsCost k n xs
