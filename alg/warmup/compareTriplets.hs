import Data.Foldable (foldl')

score :: (Ord a, Num a) => a -> a -> (a, a)
score a b
    | a > b = (1, 0)
    | a < b = (0, 1)
    | otherwise = (0, 0)

scoreN = zipWith score

scores = ((foldl' add (0,0)) .) . scoreN
    where add (a, b) (x, y) = (a + x, b + y)

loadScores = (map read) . words

showScores (a, b) = unwords [show a, show b]

main = do
    alice <- fmap loadScores getLine
    bob <- fmap loadScores getLine
    putStrLn $ showScores $ scores alice bob
