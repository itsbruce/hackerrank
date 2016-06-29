import Data.Maybe (isJust)
import Control.Monad
import Data.List (foldl')
import qualified Data.Set as Set

isSet :: Ord a => [a] -> Bool
isSet =
    let maybeNext s x =
            if Set.member x s
                then Nothing
                else Just (Set.insert x s)
        justSet = foldM maybeNext Set.empty
    in isJust . justSet
    
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
    let results = map isSet sets
    mapM_ (putStrLn . fmtOutput) results
