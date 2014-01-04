import Control.Monad (replicateM)
import Data.List (genericIndex)

facs :: [Integer]
facs = 1 : map fac [1..]
  where fac n = n * (genericIndex facs (n - 1))

factorial = genericIndex facs

teams :: Integer -> Integer -> Integer
teams _ 0 = 1
teams n k = div (factorial n) ((factorial k) * (factorial (n - k)))

runTest :: IO Integer
runTest = do
    s <- getLine
    let [n, k] = map read . take 2 $ words s
    return $ mod (teams n k) 100000007

main = do
    t <- readLn
    rs <- replicateM t runTest
    putStr . unlines $ map show rs
