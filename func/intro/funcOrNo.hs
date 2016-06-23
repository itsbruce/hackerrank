import Data.Maybe (isJust)
import Control.Monad
import Data.List (foldl')
import qualified Data.Set as Set

isSortedSet :: Ord a => [a] -> Bool
isSortedSet =
    let maybeNext ms x = do
            s <- ms
            guard (Set.notMember x s)
            return (Set.insert x s)
        justSorted = foldl' maybeNext (Just Set.empty)
    in isJust . justSorted
    
readSet :: IO [Int]
readSet = do
    n <- readLn
    lines <- replicateM n getLine
    return $ map (read . head . words) lines

fmtOutput True = "YES"
fmtOutput _ = "NO"

main = do
    n <- readLn
    sets <- replicateM n readSet
    let results = map isSortedSet sets
    mapM_ (putStrLn . fmtOutput) results
