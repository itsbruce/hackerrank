import Control.Monad (replicateM)
import qualified Data.Map.Strict as M
import Data.Word

-- Add first ocurrence
seen x p = M.insert x p

-- Remove older ocurrences 
prune p = M.filter (>= p)

enmap x p (mg, m)
    | M.notMember x m = (mg, seen x p m)
    | otherwise = case (mg, m M.! x, p - (m M.! x)) of
        (Nothing, p', g) -> (,) (Just g) $ seen x p $ prune p' m
        (Just g, p', g') -> if g' < g
                            then enmap x p (Nothing, m)
                            else (Just g, seen x p m)

minGap (g, _) [] = g
minGap (Just 1, _) _ = Just 1
minGap m ((p, x) : xs) = minGap (enmap x p m) xs

minimumDistance :: [Word32] -> Maybe Int
minimumDistance = minGap (Nothing, M.empty) . zip [1..]

showResult Nothing = "-1"
showResult (Just x) = show x

main = do
    n <- readLn
    xs <- fmap (take n . map read . words) getLine
    putStrLn $ showResult $ minimumDistance xs
