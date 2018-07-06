or35 = go [3,6..] [5,10..]
    where
        go xs@(x:xrest) ys@(y:yrest)
            | x < y = x : (go xrest ys)
            | x == y = x : (go xrest yrest)
            | otherwise = y : (go xs yrest)

sumLessThan n = sum $ takeWhile (< n) or35
