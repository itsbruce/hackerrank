import Data.Monoid
import Data.Foldable (Foldable, foldl', foldr)
import qualified Data.Vector as V
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (foldr, foldl')

default (Integer)

type AppArray = V.Vector Integer
type AppLog = [Integer]
type AppState = Cache
type App = WriterT AppLog (State AppState)

data Cache =
    Chunk {
        value:: Integer,
        lowest ::  Int,
        highest ::  Int,
        chunk :: AppArray
    } |
    Span {
        value :: Integer,
        lowest ::  Int,
        highest :: Int,
        left ::  Cache,
        right :: Cache
    } deriving Show

maxChunkSize = 8

foldLcm :: Foldable f => Integer -> f Integer -> Integer
foldLcm x = foldl' lcm x

foldLcm1 = foldLcm 1

sliceN = V.slice

sliceXY x y = sliceN x (y - x + 1)

makeCache ::  AppArray -> Cache
makeCache xs = makeCacheXY 0 (V.length xs - 1) xs

makeCacheXY :: Int -> Int -> AppArray -> Cache
makeCacheXY i i2 xs
        | xsize <= maxChunkSize = Chunk (foldLcm1 chunkSlice) i i2 chunkSlice
        | otherwise = Span spanLcm i i2 left' right'
    where   xsize = i2 - i + 1
            chunkSlice = sliceXY i i2 xs
            halves = (\(x, y) -> (x, x + y)) . (`divMod` 2)
            (lsize, rsize) = halves xsize
            left' = makeCacheXY i (i + lsize - 1) xs
            right' = makeCacheXY (i + lsize) (i + lsize + rsize - 1) xs
            spanLcm = lcm (value left') (value right')

updateSlice i f s = s V.// [(i, f (s V.! i))]

updateCache :: Int -> (Integer -> Integer) -> Cache -> Cache
updateCache i f c
        | not contains = c
        | otherwise = uc c
    where
        contains = (i >= lowest c) && (i <= highest c)
        uc (Chunk _ l h ch) =
            let ch' = updateSlice (i - l) f ch
            in  Chunk (foldLcm1 ch') l h ch'
        uc (Span _ l h lft rt) =
            let lft' = updateCache i f lft
                rt' = updateCache i f rt
            in  Span (lcm (value lft') (value rt')) l h lft' rt'

lcmInCache :: Int -> Int -> Cache -> Integer
lcmInCache x y c 
        | matches = value c
        | outside = 1
        | otherwise = lic c
    where
        matches = (x <= lowest c) && (y >= highest c)
        outside = (y < lowest c) || (x > highest c)
        lic Span {} =
            let l = lcmInCache x y $ left c
                r = lcmInCache x y $ right c
            in  lcm l r
        lic Chunk {} =
            let x' = max x (lowest c)
                y' = min y (highest c)
                i = x' - (lowest c)
                n = y' - x' + 1
            in  foldLcm1 $ sliceN i n $ chunk c

lcmInAppCache x y = do
    xs <- getCache
    return $ lcmInCache x y xs

initialState xs = makeCache xs

getCache = get

addUpdate i x = modify $ updateCache i (* x)
            
queryLcm i i2 = do
    e <- lcmInAppCache i i2
    tell [e]

data Query = Update Int Integer | LCM Int Int

doQuery (LCM x y) = queryLcm x y
doQuery (Update x y) = addUpdate x y

doQueries :: [Query] -> App ()
doQueries = mapM_ doQuery

-- solution :: [Int] -> [Query] -> [Integer]
solution xs qs =
    map mod1097 $ evalState (execWriterT (doQueries qs)) (initialState xs)
        where mod1097 = flip mod 1000000007

readInitialState :: IO AppArray
readInitialState = do
    n <- readLn
    l <- getLine
    return $ V.fromList $ map read $ take n $ words l

readQuery :: String -> Query
readQuery = go . words
    where
        go ("U" : x : y : _) = Update (read x) (read y)
        go ("Q" : x : y : _) = LCM (read x) (read y)

readQueries :: IO [Query]
readQueries = do
    n <- readLn
    qs <- replicateM n getLine
    return $ map readQuery qs

main = do
    s <- readInitialState
    qs <- readQueries
    mapM print $ solution s qs
