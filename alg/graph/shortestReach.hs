import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as M
import Data.Ord
import Data.Tuple (swap)
import qualified Data.Vector.Unboxed as V

type Edge = (Int, Int)
type EdgeMap = M.Map Int Int
type Neighbours = V.Vector Int
type AdjacentMap = M.Map Int Neighbours
data Adjacents = Adjacents { adjacents :: AdjacentMap }
type Visited = M.Map Int Bool
type Distances = M.Map Int Int
type Queue = [Int]
data BFSState = BFSState {
    visits :: Visited,
    distances :: Distances,
    queue :: Queue
}
type BFS = ReaderT Adjacents (State BFSState)

noDistance = -1
edgeLength = 6

buildAdjacents :: [Edge] -> AdjacentMap
buildAdjacents es = 
    let es' = map swap es
        es'' = foldl' (flip (:)) es es'
        p2v (k, v) = M.singleton k $ V.singleton v
        ev = map p2v es''
        merge = M.unionWith (V.++)
    in  foldl' merge M.empty ev

initialState x = BFSState {
    visits = M.empty,
    distances = M.singleton x 0,
    queue = [x]
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
    if (isV || d /=noDistance && d <= n)
        then return ()
        else modifyDistances (M.insert x n)

dequeue :: BFS Int
dequeue = do
        s <- get
        let ds = distances s
        let qs = sortByDistance $ map (addDistance ds) $ queue s
        let qs'= map fst qs
        let q = head qs'
        put s { queue = tail qs' }
        return q
    where
        addDistance ds x = (x, M.findWithDefault noDistance x ds)
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
    mapM_ (setDistance (d + edgeLength)) ns
    mapM_ enqueue ns
    setVisited x

allOtherDistances n = filterM (return . (/= 0)) =<< mapM distanceTo [1..n]

traverseGraph n = do
    done <- emptyQueue
    if done
        then allOtherDistances n
        else dequeue >>= visit >> traverseGraph n

solution n s es =
    let initialReader = Adjacents $ buildAdjacents es
    in  evalState (runReaderT (traverseGraph n) initialReader) (initialState s)

runTest =
    let makeEdge = (\[x, y] -> (x, y)) . (map read) . (take 2) . words
        readEdge = fmap makeEdge getLine
    in  do
            [n, m] <- fmap ((take 2) . (map read) . words) getLine
            es <- replicateM m readEdge
            s <- readLn
            return $ unwords $ (map show) $ solution n s es

main = do
    n <- readLn
    results <- replicateM n runTest
    mapM_ putStrLn results
