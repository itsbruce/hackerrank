import Control.Monad
import qualified Data.Map.Strict as M
import Data.List

type Primes = M.Map Int Int

fromString :: String -> Primes
fromString = M.fromList . go . (map read) . words
    where
        go [] = []
        go (k:v:rest) = (k, v) : go rest

toString :: Primes -> String
toString = unwords . (map show) . go . M.toAscList
    where
        go [] = []
        go ((k, v):rest) = k : v : go rest

gcdPrimes :: Primes -> Primes -> Primes
gcdPrimes = M.intersectionWith min

listGcd = foldl1' gcdPrimes

main = do
    n <- readLn
    xs <- replicateM n $ fmap fromString getLine
    putStrLn $ toString $ listGcd xs
