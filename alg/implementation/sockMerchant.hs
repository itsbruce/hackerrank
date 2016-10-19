import qualified Data.Set as S

data Match a f = Matched a (f a) | NoMatch (f a)
    deriving (Eq, Show)

match :: Int -> S.Set Int -> Match Int S.Set
match x xs
    | S.member x xs = Matched x $ S.delete x xs
    | otherwise     = NoMatch $ S.insert x xs

pairs :: [Int] -> [Int]
pairs = go S.empty
    where
        go _ [] = []
        go xset (x:xs) = go' (match x xset) xs
        go' (Matched x xset) xs = x : go xset xs
        go' (NoMatch xset) xs = go xset xs

countPairs = length . pairs

main = do
    n <- readLn
    xs <- fmap (map read . take n . words) getLine
    print $ countPairs xs
