import Control.Monad (mfilter, replicateM)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Data.Maybe (isJust)

type Tally = Map.Map Char Int

count :: Char -> Tally -> Tally
count c t = Map.adjust (succ) c t

diff :: Char -> Char -> Tally -> Int
diff x y t = abs $ (t ! x) - (t ! y)

level x y t = 0 == diff x y t

allLevel t = (level 'R' 'G' t) && (level 'Y' 'B' t)

close x y t = 2 > abs (diff x y t)

allClose t = (close 'R' 'G' t) && (close 'Y' 'B' t)

full :: String -> Bool
full = let  seed = Map.fromList [('R', 0), ('G', 0), ('Y', 0), ('B', 0)]
            maybeNext mt x = mfilter allClose mt >> fmap (count x) mt
            tally = foldl' maybeNext $ Just seed
        in isJust . (mfilter allLevel) . tally

main = do
    n <- readLn
    xs <- replicateM n getLine
    mapM putStrLn $ map (show . full) xs
