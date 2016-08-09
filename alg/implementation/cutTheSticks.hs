import Data.List (sort, unfoldr)

cuts = unfoldr step . sort
    where
        cut (x : xs) = map (subtract x) $ dropWhile (== x) xs
        step [] = Nothing
        step xs = Just $ (length xs, cut xs)
        
main = do
    n <- readLn
    xs <- fmap (take n . map read . words) getLine
    mapM print $ cuts xs
