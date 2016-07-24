import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Data.Vector.Unboxed as V

type Square = Int
type Jump = (Square, Square)
type JumpMap = M.Map Square Square
type Neighbours = V.Vector Square
type AdjacentMap = M.Map Square Neighbours
data Adjacents = Adjacents { adjacents :: AdjacentMap }
type Visited = M.Map Square Bool
type Distances = M.Map Square Square
type Queue = [Square]
data BFSState = BFSState {
    visits :: Visited,
    distances :: Distances,
    queue :: Queue
}
--type BFS = ReaderT Adjacents (State BFSState)
type BFS = ReaderT Adjacents (State BFSState)

start = 1
goal = 100
maxRoll = 6
noDistance = 10000

jumps :: [Jump] -> JumpMap
jumps = M.fromList

buildAdjacents :: JumpMap -> AdjacentMap
buildAdjacents = \js -> M.fromList $ concatMap (getNeighbours js) [start..goal]
    where
        getNeighbours js n
            | M.member n js = []
            | otherwise = [(n, neighbours js n)]
        neighbours = 
            let moves n = [(n+1)..(min 100 (n + maxRoll))]
                lookupJump js n =  M.findWithDefault n n js
                lookup js = V.fromList . (map (lookupJump js))
            in  \js n -> lookup js $ moves n

initialState = BFSState {
    visits = M.empty,
    distances = M.singleton 1 0,
    queue = [1]
}

visited x = return . find =<< fmap visits get
    where
        find = M.findWithDefault False x

distanceTo x = return . find =<< fmap distances get
    where
        find = M.findWithDefault noDistance x

neighbours x = return . find =<< fmap adjacents ask
    where
        find as = as M.! x

modifyDistances f = do
    s <- get
    let ds = distances s
    put s { distances = f ds }

setVisited x = do
    s <- get
    let vs = visits s
    put s { visits = M.insert x True vs }

setDistance n x = do
    isV <- visited x
    d <- distanceTo x
    if (isV || d <= n)
        then return ()
        else modifyDistances (M.insert x n)

dequeue :: BFS Square
dequeue = do
        s <- get
        let ds = distances s
        let qs = sortByDistance $ map (addDistance ds) $ queue s
        let qs'= map fst qs
        let q = head qs'
        put s { queue = tail qs' }
        return q
    where
        addDistance ds x = (x, ds M.! x)
        sortByDistance = sortBy (comparing snd)

inQueue x = do
    s <- get
    let qs = queue s
    return $ elem x qs

emptyQueue = do
    s <- get
    let qs = queue s
    return $ null qs

enqueue x =
    let prepend = do
        s <- get
        let qs = queue s
        put s { queue = x : qs }
    in  do
            isV <- visited x
            inQ <- inQueue x
            if (isV || inQ)
                then return ()
                else prepend
    
visit x = do
    d <- distanceTo x
    ns <- fmap (V.toList) (neighbours x)
    mapM_ (setDistance (d+1)) ns
    mapM_ enqueue ns
    setVisited x

minDistanceToGoal = do
    gv <- visited goal
    failed <- emptyQueue
    if failed
        then setDistance (-1) goal
        else return ()
    if (gv || failed)
        then distanceTo goal
        else dequeue >>= visit >> minDistanceToGoal

solution js =
    let initialReader = Adjacents $ buildAdjacents $ jumps js
        --work = evalStateT minDistanceToGoal initialState
    in  evalState (runReaderT minDistanceToGoal initialReader) initialState

runTest =
    let makeJump = (\[x, y] -> (x, y)) . (map read) . words
        readJumps = do
            n <- readLn
            replicateM n $ fmap makeJump getLine
    in  do
            snakes <- readJumps
            ladders <- readJumps
            return $ solution $ snakes ++ ladders

main = do
    n <- readLn
    results <- replicateM n runTest
    mapM_ putStrLn $ map show results
