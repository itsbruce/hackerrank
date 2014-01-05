import Text.Regex.PCRE

main = do
    n <- readLn
    c <- getContents
    putStr $ unlines $ filter (=~ "(?i)^HI [^d]") $ take n $ lines c
