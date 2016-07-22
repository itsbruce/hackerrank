convert [h0, h1, c0, m0, m1, c1, s0, s1, a, m] =
    let rest = [c0, m0, m1, c1, s0, s1]
        hrs 'P' 12 = "12"
        hrs 'P' h = show $ h + 12
        hrs 'A' 12 = "00"
        hrs _ 10 = "10"
        hrs _ 11 = "11"
        hrs _ h = '0' : (show h)
        hours a h = hrs a $ read h
    in  (hours a [h0, h1]) ++ rest

main = putStrLn =<< fmap convert getLine
