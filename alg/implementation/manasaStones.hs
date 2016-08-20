import Control.Applicative
import Control.Monad (replicateM)
import qualified Data.Set as S

stones :: Int -> Int -> Int -> [Int]
stones 0 _ _ = error "Zero stones!"
stones n x y = stone n [0]
    where
        xs = [x,y]
        stone 1 ys = ys
        stone x ys = stone (x - 1) $ S.toList $ S.fromList $ (+) <$> xs <*> ys

runTest = do
    n <- readLn
    x <- readLn
    y <- readLn
    return $ stones n x y

showTest = putStrLn . unwords . map show

main = do
    t <- readLn
    results <- replicateM t runTest
    mapM showTest results
