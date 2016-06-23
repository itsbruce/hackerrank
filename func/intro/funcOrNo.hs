{-
 - This solution works only with ordered lists
 - which was a mistaken assumption about the input
 -}
import Data.Maybe (isJust)
import Control.Monad
import Data.List (foldl1')

isSortedSet :: Ord a => [a] -> Bool
isSortedSet =
    let maybeNext mx my = do
            x <- mx
            y <- my
            guard (x < y)
            return y
        justSorted = (foldl1' maybeNext) . (map Just)
    in isJust . justSorted
    
readSet :: IO [Int]
readSet = do
    n <- readLn
    lines <- replicateM n getLine
    return $ map (read . head . words) lines

fmtOutput True = "Yes"
fmtOutput _ = "No"

main = do
    n <- readLn
    sets <- replicateM n readSet
    let results = map isSortedSet sets
    mapM_ (print . fmtOutput) results
