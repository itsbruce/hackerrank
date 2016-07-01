import Control.Monad

valid :: [Int] -> Bool
valid xs = case xs of
                [] -> True
                [x] -> True
                (x:xs) -> let left = takeWhile (< x) xs
                              right = drop (length left) xs
                          in (valid left) && (all (> x) right) && (valid right)

yesNo :: Bool -> String
yesNo b = if b then "YES" else "NO"

main = do
    t <- readLn
    rs <- replicateM t $ do
        n <- readLn
        s <- getLine
        return . yesNo . valid . map read . take n $ words s
    putStr $ unlines rs
