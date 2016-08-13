import Data.List (foldl')
import qualified Data.Map.Strict as M

ksum k 0 = (== 0)
ksum k n = (== k - n)

anyksum k [] = False
anyksum k [_] = False
anyksum k (x:xs) = any (ksum k x) xs || anyksum k xs

seed k = M.fromList $ zip [0..k-1] $ repeat 0

inc m i = M.alter (fmap succ) i m

makeMap k = foldl' inc (seed k)

justOne = M.alter (fmap signum) 

limit0 = justOne 0

limitHalf k
    | even k = justOne $ div k 2
    | otherwise = id

limits k = limit0 . limitHalf k

maxNonDiv k xs = 
    let kmap = limits k $ makeMap k $ map (flip mod k) xs
        total n = kmap M.! n
        totalN n = max (total n) (total (k - n))
        total0 = kmap M.! 0
        totals = map totalN [1 .. div k 2]
    in  total0 + sum totals

main = do
        [n, k] <- readN 2
        xs <- readN n
        print $ maxNonDiv k xs
    where
        readN n = fmap (take n . map read . words) getLine
