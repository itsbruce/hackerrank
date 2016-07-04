import Data.Monoid
import Data.Foldable (Foldable, foldMap)
import qualified Data.Vector as V
-- import Control.Monad.Trans.State.Strict
import Control.Monad.State
import Control.Monad.Writer
import Data.Int
--import Control.Monad.Trans

newtype MinMult a = MinMult { getMinMult :: a }
    deriving (Eq, Ord, Read, Show)

instance (Integral a) => Monoid (MinMult a) where
    mempty = MinMult 1
    mappend (MinMult x) (MinMult y) = MinMult (lcm x y)

type AppStore = V.Vector Integer
type AppState a = StateT AppStore (Writer [Integer]) a

loadState :: [Int] -> AppStore
loadState = V.fromList . (map fromIntegral)

updateState :: Int -> Integer -> AppState ()
updateState i x = modify updateV
    where updateV v = V.update v (V.singleton (i, x * (v V.! i)))
            
lcmSlice :: Int -> Int -> AppState Integer
lcmSlice i i2 = do
    v <- get
    let s = V.slice i (i2 - i + 1) v
    -- return $ getMinMult $ foldMap MinMult s
    return $ V.foldr lcm 1 s

querySlice i i2 = do
    x <- lcmSlice i i2
    lift $ tell [x]
    -- lift $ tell [fromIntegral x]

data Query = Update Int Integer | LCM Int Int

doQuery (LCM x y) = querySlice x y
doQuery (Update x y) = updateState x y

doQueries = mapM doQuery

solution :: [Int] -> [Query] -> [Integer]
solution xs qs =
    map mod1097 $ execWriter $ evalStateT (doQueries qs) $ loadState xs
    -- execWriter $ evalStateT (doQueries qs) $ loadState xs
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
