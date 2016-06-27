import Control.Monad

swap :: [a] -> [a]
swap [] = []
swap [x] = [x]
swap (x : y: rest) = y : x : (swap rest)

main = do
    n <- readLn
    input <- replicateM n getLine
    mapM_ putStrLn $ map swap input
