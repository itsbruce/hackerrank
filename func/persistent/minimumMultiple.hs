--{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE FlexibleContexts #-}
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Foldable (Foldable, fold, foldl', foldMap, foldr)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad (liftM2)
-- import Control.Monad.State
-- import Control.Monad.Writer
import Control.Monad.RWS
import Prelude hiding (foldr, foldl')

default (Integer)

{-
newtype MinMult a = MinMult { getMinMult :: a }
    deriving (Eq, Ord, Read, Show)

instance Integral a => Monoid (MinMult a) where
    mempty = MinMult 1
    mappend (MinMult x) (MinMult y) = MinMult (lcm x y)
-}

type AppArray = V.Vector Int
type AppEnv = (AppArray, Cache)
type AppLog = [Integer]
type Updates = M.Map Int Integer
type AppState = Updates
type App a = RWS AppEnv AppLog AppState a

-- data Empty
-- data Branches = Empty | Range Range
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

foldLcm :: (Functor f, Foldable f, Integral a) => Integer -> f a -> Integer
foldLcm x = (foldl' lcm x) . (fmap fromIntegral)

foldLcm1 :: (Functor f, Foldable f, Integral a) =>  f a -> Integer
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

loadEnv :: [Int] -> AppEnv
loadEnv xs = (loadArray, loadCache)
    where   loadArray = V.fromList $ map fromIntegral xs
            loadCache = makeCache loadArray

initialState = M.empty

getArray = fmap fst ask

getCache :: App Cache
getCache = fmap snd ask

getUpdates :: App Updates
getUpdates = get

modifyUpdates f = modify f

lookupE :: Int -> App (Maybe Int)
lookupE i = fmap (V.!? i) getArray

lookupU :: Int -> App (Maybe Integer)
lookupU i = fmap (M.lookup i) getUpdates

addU :: Int -> Integer -> App ()
addU i x = modifyUpdates (M.insert i x)

lookupVal :: Int -> App Integer
lookupVal i = do
    u <- lookupU i
    e <- lookupE i
    let e' = fmap toInteger e
    return $ fromJust $ getFirst $ (First u) <> (First e')

addUpdate :: Int -> Integer -> App ()
addUpdate i x = do
    v <- lookupVal i
    addU i (v * x)
            
updatesInRange i i2 = do
    xs <- getUpdates
    return $ M.filterWithKey (\k _ -> (k >= i) && (k <= i2)) xs

queryLcm i i2 = do
    e <- lcmInAppCache i i2
    xs <- updatesInRange i i2
    let x = foldLcm e xs
    tell [x]

data Query = Update Int Integer | LCM Int Int

doQuery (LCM x y) = queryLcm x y
doQuery (Update x y) = addUpdate x y

doQueries = mapM doQuery

solution :: [Int] -> [Query] -> [Integer]
solution xs qs =
    map mod1097 $ snd $ evalRWS (doQueries qs) (loadEnv xs) initialState
        where mod1097 = flip mod 1000000007

readInitialState :: IO [Int]
readInitialState = do
    n <- readLn
    l <- getLine
    return $ map read $ take n $ words l

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
