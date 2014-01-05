import Data.List

changes ::  [Char] -> Int
changes xs = case mod (length xs) 2 of
    1 -> -1
    _ -> let (left, right) = splitAt (length xs `div` 2) xs
         in changes' left right

changes' _ [] = 0
changes' xs ys = count + changes' xs' ys'
    where match = (== head ys)
          num = length . filter match
          xnum = num xs
          ynum = num ys
          count = if (ynum > xnum) then ynum - xnum else 0
          fnot = filter (not . match)
          xs' = fnot xs
          ys' = fnot ys

main = do
    n <- readLn
    c <- getContents
    putStr $ unlines $ map show $ map changes $ take n $ lines c
