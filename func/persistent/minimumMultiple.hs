-- {-# LANGUAGE TupleSections #-}
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Foldable (Foldable, fold, foldl', foldMap, foldr)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
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

type AppEnv = V.Vector Integer
type Updates = M.Map Int Integer
type AppLog = [Integer]
type App a = RWS AppEnv AppLog Updates a

loadEnv :: [Int] -> AppEnv
loadEnv = V.fromList . (map fromIntegral)

initialState = M.empty

getUpdates :: App Updates
getUpdates = get

lookupE :: Int -> App (Maybe Integer)
lookupE i = fmap (V.!? i) ask

lookupU :: Int -> App (Maybe Integer)
lookupU i = fmap (M.lookup i) getUpdates

addU :: Int -> Integer -> App ()
addU i x = modify (M.insert i x)

lookupVal :: Int -> App Integer
lookupVal i = do
    u <- lookupU i
    e <- lookupE i
    return $ fromJust $ getFirst $ (First u) <> (First e)

addUpdate :: Int -> Integer -> App ()
addUpdate i x = do
    v <- lookupVal i
    addU i (v * x)
            
envLcm :: Int -> Int -> App Integer
envLcm  = (fmap lcmSlice .) . takeSlice
    where
        lcmSlice = foldl' lcm 1
        takeSlice i i2 = (return . (V.slice i (i2 - i + 1))) =<< ask

updatesInRange i i2 = do
    xs <- getUpdates
    return $ M.filterWithKey (\k _ -> (k >= i) && (k <= i2)) xs

queryLcm i i2 = do
    e <- envLcm i i2
    xs <- updatesInRange i i2
    let x = foldl' lcm e xs
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
