import Control.Applicative
import Control.Monad (replicateM)
import Data.Bits


xorSubs :: (Num a, Bits a) => [a] -> [a]
xorSubs [] = [0]
xorSubs (x : xs) = [flip const x,xor x] <*> xorSubs xs 

runTest :: IO Int
runTest = do
    n <- readLn
    xs <- fmap (take n . map read . words) getLine
    return $ sum $ xorSubs xs

main = do
        t <- readLn
        results <- replicateM t runTest
        mapM print $ map mod1097 results
    where 
        mod1097 = flip mod (10^9 + 7)
