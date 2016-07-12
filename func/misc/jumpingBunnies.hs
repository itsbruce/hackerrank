import Data.Foldable (foldl')

solution :: [Integer] -> Integer
solution = foldl' lcm 1

main = do
    n <- readLn
    xs <- fmap ((map read) . (take n) . words) getLine
    print $ solution xs
