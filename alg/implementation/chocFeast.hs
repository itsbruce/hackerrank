import Control.Monad (replicateM)

chocs n c m =
    let chocs = div n c
        swap c
            | c >= m = (\(d, d') -> d + swap (d + d')) $divMod c m
            | otherwise = 0
    in  chocs + swap chocs

trip = do
    [n, c, m] <- fmap (take 3 . map read . words) getLine
    return $ chocs n c m

main = mapM print =<< flip replicateM trip =<< readLn
