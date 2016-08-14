import Control.Monad (replicateM)
import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Map.Lazy as M

modMap n = M.fromList $ map (fmap (mod n)) $ zip [1..9] [1..9]

evenDivs :: String -> Int
evenDivs ns =
    let n = read ns
        mm = modMap n
        ns' = map digitToInt ns
        tally a b
            | b > 0 && mm M.! b == 0 = a + 1
            | otherwise = a
    in foldl' tally 0 ns'

runTest = fmap evenDivs getLine

main = do
    t <- readLn
    xs <- replicateM t runTest
    mapM print xs
