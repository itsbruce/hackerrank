import Control.Monad (replicateM)
import Data.List (foldl')

data Coord = Coord Int Int deriving Show

readCoord :: String -> Coord
readCoord s =
    let [first, second] = map read $ words s
    in  Coord first second

area :: [Coord] -> Float
area cs =
    let first = head cs
        rest = tail cs
        seed = (first, 0)
        step (Coord x y, n) c2@(Coord x1 y1) =
            (c2, n + (x * y1) - (y * x1))
        tally =  fromIntegral $ snd $ step (foldl' step seed rest) first
    in  tally / 2

main = do
    n <- readLn
    ls <- replicateM n getLine
    putStrLn $ show $ area $ map readCoord ls
