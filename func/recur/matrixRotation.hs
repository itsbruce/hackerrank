import Control.Monad.Reader
import Data.Map.Strict as M


{- To rotate the matrix anti-clockwise, we move clockwise from
 - each cell, fetch the value from that point and place it in
 - the start location
 -}

data Direction = Across | Down | Back | Up
    deriving (Enum, Show)

type Distance = Int
type Location = (Int, Int)
type Matrix = M.Map (Int, Int) String
type App = Reader (Distance, Distance)


width = 5
depth = 4

nextDirection Up = Across
nextDirection x = succ x

dimension Across = width
dimension Back = width
dimension _ = depth

room size (x, y)
    | x < y = 0
    | otherwise =  max 0 $ size - 1 - x - y

asIfAcross Across l = l
asIfAcross Down (x, y) = (y, width - x - 1)
asIfAcross Back (x, y) = (width - x - 1, depth - y - 1)
asIfAcross Up (x, y) = (depth - y - 1, x)

roomIn d l = room (dimension d) (asIfAcross d l)


travel Across n (x, y) = (x + n, y)
travel Back n (x, y) = (x - n, y)
travel Down n (x, y) = (x, y + n)
travel Up n (x, y) = (x, y - n)

move :: (Direction, Distance, Location) -> (Direction, Distance, Location)
move x@(_, 0, l) = x
move x@(d, n, l)
        | n < roomForX = (d, max 0 (n - roomForX), go n)
        | otherwise = (nextDirection d, n - roomForX, go roomForX)
    where
        roomForX = roomIn d l
        go x = travel d x l

clockwise n l = until done move (Across, n, l)
    where
        done (_, 0, _) = True
        done _ = False

