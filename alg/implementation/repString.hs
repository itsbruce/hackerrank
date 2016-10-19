import qualified Data.Vector.Unboxed as V

acounts :: [Char] -> [Int]
acounts = scanl go 0
    where
        go n 'a' = n + 1
        go n _ = n

asInN :: Int -> [Char] -> Int
asInN n xs =
    let counts = V.fromList $ acounts xs
        size = pred $ V.length counts
        (reps, slice) = divMod n size
        atotal = V.last counts
        aslice = counts V.! slice
    in (reps * atotal) + aslice

main = do
    xs <- getLine
    n <- fmap read getLine
    print $ asInN n xs
