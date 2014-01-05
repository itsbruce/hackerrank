import Text.Regex.PCRE

score :: String -> Int
score s = case (match1, match2) of
            (0, 1) -> 2
            (1, 0) -> 1
            (1, 1) -> 0
            _ -> -1
    where match1 = s =~ "^hackerrank" :: Int
          match2 = s =~ "hackerrank$" :: Int
    

main = do
    n <- readLn
    c <- getContents
    putStr $ unlines $ map show $ map score $ take n $ lines c
