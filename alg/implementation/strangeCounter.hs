
strange = go 3 . dm 3
    where
        dm n = wrap n . flip divMod n
        wrap n (d, 0) = (d - 1, n)
        wrap n x = x
        go n (0, x) = (n - x) + 1
        go n (d, x) = go (2 * n) $ dm (2 * n) $ (d - 1) * n + x

main = fmap strange readLn >>= print
    
