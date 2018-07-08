import Control.Monad
import Data.Monoid (mconcat, Last(..))

primes = 2 : 3 : sieve (tail primes) [5,7..]
    where
        sieve (p:ps) xs = (++) underpp $ sieve ps $ notp overpp
            where
                (underpp, ~(_:overpp)) = span (< p * p) xs
                notp = filter ((/=0) . (`rem` p))

largestpf n = 
    let primesuspects = takeWhile ((<= n) . (* 2)) primes
        candidates = filter ((== 0) . (mod n)) primesuspects
        largest = getLast $ mconcat $ map (Last . Just) candidates
    in  foldr const n largest

tests :: Int -> IO [Integer]
tests n = replicateM n readLn

main = do
    t <- readLn
    ts <- tests t
    let results = map largestpf ts
    mapM (putStrLn . show) results
