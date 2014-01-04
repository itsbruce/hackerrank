rev = foldl (flip (:)) []



main = do
		inputdata <- getContents
		mapM_ putStrLn $ map show $ rev $ map (read :: String -> Int) $ lines inputdata
