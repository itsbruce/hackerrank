-- import Control.Monad (return)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : rest) = x : unique (filter (/= x) rest)

main = putStrLn =<< fmap unique getLine

