addTwo = (+)

main = do
    a <- readLn
    b <- readLn
    putStrLn $ show $ addTwo a b
