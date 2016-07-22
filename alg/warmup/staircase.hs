staircase n = take n $ zipWith step blanks hashes
    where
        blanks = [n-1,n-2..]
        hashes = [1..n]
        step b h = (replicate b ' ') ++ (replicate h '#')

main = (mapM putStrLn) =<< fmap staircase readLn

