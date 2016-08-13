import Control.Monad (replicateM)

data Growing a = Double { height :: a } | PlusOne { height :: a }

grow :: Int -> Int
grow = go (Double 1)
    where
        go g 0 = height g
        go g n = go (grow g) (n - 1)
        grow (Double x) = PlusOne (2 * x)
        grow (PlusOne x) = Double (x + 1)

runTest = fmap grow readLn >>= print
        
main = do
    t <- readLn
    replicateM t runTest
