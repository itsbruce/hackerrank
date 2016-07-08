--import Data.Monoid
import Data.Foldable -- (Foldable, fold, foldMap, foldr)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (foldr)

default (Integer)

{-
newtype MinMult a = MinMult { getMinMult :: a }
    deriving (Eq, Ord, Read, Show)

instance Integral a => Monoid (MinMult a) where
    mempty = MinMult 1
    mappend (MinMult x) (MinMult y) = MinMult (lcm x y)
-}

type AppArray = V.Vector Integer
type AppStore = AppArray
type AppLog = [Integer]
type AppState a = StateT AppStore (Writer AppLog) a

loadState :: [Int] -> AppStore
loadState = V.fromList . (map fromIntegral)

getArray :: AppState AppArray
getArray = get

updateState :: Int -> Integer -> AppState ()
updateState = (modify . ) . updateV
    where
        updateV i x v = V.update v (V.singleton (i, x * (v V.! i)))
            
lcmSliceM :: Int -> Int -> AppState Integer
lcmSliceM = (fmap lcmSlice .) . takeSlice
    where
        lcmSlice = foldl' lcm 1
        takeSlice i i2 = (return . (V.slice i (i2 - i + 1))) =<< getArray

querySlice i i2 = do
    x <- lcmSliceM i i2
    lift $ tell [x]

data Query = Update Int Integer | LCM Int Int

doQuery (LCM x y) = querySlice x y
doQuery (Update x y) = updateState x y

doQueries = mapM doQuery

solution :: [Int] -> [Query] -> [Integer]
solution xs qs =
    map mod1097 $ execWriter $ evalStateT (doQueries qs) $ loadState xs
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
