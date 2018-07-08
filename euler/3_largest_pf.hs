import Control.Monad

primes = 2 : 3 : sieve (tail primes) [5,7..]
    where
        sieve (p:ps) xs = (++) underpp $ sieve ps $ notp overpp
            where
                (underpp, ~(_:overpp)) = span (< p * p) xs
                notp = filter ((/=0) . (`rem` p))

largestpf n = 
    let primesuspects = takeWhile ((<= n) . (* 2)) primes
        try _ biggest [] = biggest
        try top biggest ps@(p:rest)
            | mod top p == 0 = try (div top p) (Just p) $ takeWhile ((<= top) . (*  p)) ps
            | otherwise = try top biggest rest
        largest = try n Nothing primesuspects
    in  foldr const n largest

tests :: Int -> IO [Integer]
tests n = replicateM n readLn

main = do
    t <- readLn
    ts <- tests t
    let results = map largestpf ts
    mapM (putStrLn . show) results
