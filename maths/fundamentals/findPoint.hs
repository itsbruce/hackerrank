import Control.Monad (replicateM)

findPoint :: (Int, Int) -> (Int, Int) -> (Int, Int)
findPoint (px, py) (qx, qy) = (qx + (qx - px), qy + (qy - py))

findPoints = do
    [px, py, qx, qy] <- fmap (take 4 . map read . words) getLine
    return $ findPoint (px, py) (qx, qy)

showPoint (x, y) = putStrLn $ unwords $ map show [x,y]

main = do
    t <- readLn
    results <- replicateM t findPoints
    mapM showPoint results
