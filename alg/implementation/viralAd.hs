import Data.Word
import Data.List (unfoldr)

likedOverTime :: Word8 -> Integer
likedOverTime = sum . unfoldr go . (,) 5 . toInteger
    where
        likedToday = flip div 2
        shared = (* 3)
        go (_, 0) = Nothing
        go (x, n) =
            let x' = likedToday x
            in  Just (x', (shared x', n - 1))

main = print =<< fmap (likedOverTime . read) getLine
