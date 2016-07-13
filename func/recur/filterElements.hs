-- {-# LANGUAGE TupleSections #-}
import Control.Monad (mapM, replicateM)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

type Counts = M.Map Int Int
type Indices = M.Map Int Int
type App a = State (Counts, Indices, Int) a

getCounts :: App Counts
getCounts = fmap (\(cs, _, _) -> cs) get

getCount c = fmap (M.findWithDefault 0 c) getCounts

modifyCounts f = do
    (cs, is, n) <- get
    put (f cs, is, n)


modifyIndices f = do
    (cs, is, n) <- get
    put (cs, f is, n)

getPosition :: App Int
getPosition = fmap (\(_, _, n) -> n) get

bumpPosition = do
    (cs, is, n) <- get
    put (cs, is, n + 1)

tally c = do
        n <- getCount c
        bump (n + 1) c
        i <- getPosition
        bumpPosition
        if n == 0
            then firstAt i c
            else return ()
    where
        bump n k = modifyCounts (M.alter (const (Just n)) k)
        firstAt n x = modifyIndices (M.insert n x)

kOrMore k = do
    (cs, is, _ ) <- get
    let xs = M.filter (\a -> k <= cs M.! a) is
    return $ map snd $ M.toAscList xs
    
    
-- solution :: Int -> [Char] -> [Char]
solution k xs = evalState ((mapM tally xs) >> (kOrMore k)) seed
    where
        seed = (M.empty, M.empty, 0)

solutionIO :: IO String
solutionIO = do
        [n, k] <- fmap ((map read) . words) getLine
        xs <- fmap ((take n) . (map read) . words) getLine
        return $ format $ solution k xs
    where
        format [] = "-1"
        format is = unwords $ map show is

main = do
    n <- readLn
    xs <- replicateM n solutionIO
    mapM putStrLn xs
