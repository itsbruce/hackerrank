import Control.Monad (replicateM)
import Data.List (foldl')

data Coord = Coord { x :: Int, y :: Int } deriving Show

distance :: Coord -> Coord -> Float
distance c1 c2
    | xdiff == 0 = fromIntegral ydiff
    | ydiff == 0 = fromIntegral xdiff
    | otherwise = sqrt $ fromIntegral $ (xdiff ^ 2) + (ydiff ^ 2)
    where   xdiff = diff x
            ydiff = diff y
            diff f = (max (f c1) (f c2)) - (min (f c1) (f c2))
            
readCoord :: String -> Coord
readCoord s = let [first, second] = map read $ words s
              in  Coord first second

circumference :: [Coord] -> Float
circumference cs =
    let start = (head cs, 0.0)
        step (c1, n) c2 = (c2, n + (distance c1 c2))
    in  snd $ foldl' step start $ reverse cs

main = do
    n <- readLn
    ls <- replicateM n getLine
    putStrLn $ show $ circumference $ map readCoord ls
