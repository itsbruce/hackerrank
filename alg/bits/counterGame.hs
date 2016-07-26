import Control.Monad
import Data.Bits
import Data.List (unfoldr)

data Player = Louise | Richard deriving Show

next Louise = Richard
next Richard = Louise

shiftMaybe 0 = Nothing
shiftMaybe x = Just (x, shift x (-1))

bitCount = length . (unfoldr shiftMaybe)

reduce :: Integer -> Integer
reduce x =
    let n = bitCount x
        x' = shift 1 (n - 1)
        x'' = shift 1 (n - 2)
    in  if x == x'
        then x''  
        else x - x'

turn p 1 = next p
turn p x = turn (next p) $ reduce x

game x = turn Louise x

main = do
    t <- readLn
    ts <- replicateM t readLn
    mapM_ print $ map game ts
