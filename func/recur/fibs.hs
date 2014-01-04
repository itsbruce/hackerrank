fib = (!!) fibs . pred
    where fibs = 0 : 1 : map f [2..]
          f n = (fibs !! (n - 2)) + (fibs !! (n - 1))

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ input
