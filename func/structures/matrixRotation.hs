import Control.Monad.Reader
import qualified Data.Map.Strict as M


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


askWidth :: App Distance
askWidth = fmap fst ask

askDepth :: App Distance
askDepth = fmap snd ask

nextDirection Up = Across
nextDirection x = succ x

-- Flip dimensions for calculating Up/Down movement
dimensions Up = (\(w, d) -> (d, w))
dimensions Down = dimensions Up
dimensions _ = id

-- Space available to move across for a given location
room (w, d) (x, y)
      | x < y = 0
      | y >= d - (div d 2) = 0
      | otherwise =  max 0 $ w - 1 - x - y

{- Rotate a location which moves in a given direction to the equivalent
 - position which could move Across
 -}
asIfAcross Across l = return l
asIfAcross Down (x, y) = askWidth >>= ((\w -> return (y, w - x - 1)))
asIfAcross Back (x, y) = ask >>= (\(w, d) -> return (w - x - 1, d - y - 1))
asIfAcross Up (x, y) = askDepth >>= (\d -> return (d - y - 1, x))

-- Space a location can move in a given direction
roomIn d l = do
    l' <- asIfAcross d l
    wd <- ask
    return $ room (dimensions d wd) l'

-- Move in a given direction (does not check limis)
travel Across n (x, y) = (x + n, y)
travel Back n (x, y) = (x - n, y)
travel Down n (x, y) = (x, y + n)
travel Up n (x, y) = (x, y - n)

{- Move a locaton as far as is posssible in the desired direction, return
 - the new location with the unspent Distance and the next direction
 -}
move :: (Direction, Distance, Location) -> App (Direction, Distance, Location)
move m@(_, 0, l) = return m
move m@(d, n, l) = do
        roomForX <- roomIn d l
        if n < roomForX
            then return (d, max 0 (n - roomForX), go n)
            else return (nextDirection d, n - roomForX, go roomForX)
    where
        go x = travel d x l

-- Execute as many moves as necessary to travel the distance required
moveTillDone x@(_, 0, l) = return x
moveTillDone x = moveTillDone =<< move x

-- Which rectangle/square is a location in? (0 is the outside)
layer = \(x, y) -> do
        (w, d) <- ask
        return $ min (layer' w x) (layer' d y)
    where
        layer' w x =
            let halve n = (\(x, y) -> x + y) $ divMod n 2
                h = halve w
                i = div x h
            in  [x, w - x - 1] !! i

-- Circumference of layer in which location falls
circumference l = do
        (w, d) <- ask
        n <- layer l
        return $ (w + d) * 2 - 4 - n * 8

-- Move clockwise starting with Across
clockwise n l = do
    c <- circumference l            -- Calculate circumference of layer
    let n' = mod n c                -- 
    (_, _, l') <- moveTillDone (Across, n', l)
    return l'

rotateMatrix n m w d = M.mapWithKey getClockwise m
    where getClockwise k = const (m M.! (runReader (clockwise n k) (w, d)))
    --where getClockwise k = const $ runReader (fmap show (layer k)) (w, d)

loadMatrix = M.fromList . concat . (zipWith addXY [0..])
    where
        addXY y = zip (zip [0..] (repeat y))

readMatrix y x = replicateM y $ fmap ((take x) . words) getLine

unloadMatrix n m = 
    map (\i -> unwords (M.elems (M.filterWithKey (\(_, y) _ -> y == i) m))) [0..(n-1)] 

main = do
    [y, x, n] <- fmap ((take 3) . (map read) . words) getLine
    xs <- fmap loadMatrix (readMatrix y x)
    mapM putStrLn $ unloadMatrix y $ rotateMatrix n xs x y
