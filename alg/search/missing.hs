import Data.List (foldl')
import Data.Map (differenceWith,empty,keys,insertWith',Map)
import qualified Data.ByteString.Char8 as BC

freqs :: Ord a => [a] -> Map a Int
freqs = foldl' (flip f) empty
    where f k = insertWith' (+) k 1

missing :: Ord a => [a] -> [a] -> [a]
missing as bs = keys $ differenceWith f (freqs bs) (freqs as)
    where f b a | a == b = Nothing
                | otherwise = Just (b - a)

main = do as <- getList
          bs <- getList
          putStrLn $ unwords $ map show $ missing as bs
    where
        getList :: IO [Int]
        getList = do
                n <- readLn
                s <- BC.getLine
                return . map readI . take n $ BC.words s
        readI s = case BC.readInt s of
                    Just (i, _) -> i
                    Nothing -> 0
