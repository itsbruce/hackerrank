--Only fill up the blanks for the function named len
--Do not modify the structure of the template in any other way
-- Head ends here
len = foldr (\_ i -> succ i) 0

main = do
		inputdata <- getContents
		putStrLn $ show $ len $ map (read :: String -> Int) $ lines inputdata
