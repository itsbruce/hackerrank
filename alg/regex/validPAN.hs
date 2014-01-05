import Text.Regex.PCRE

pan :: String -> String
pan s = if s =~ p then "YES" else "NO"
    where p = "[[:upper:]]{5}[[:digit:]]{4}[[:upper:]]{1}"

main = do
    n <- readLn
    c <- getContents
    putStr $ unlines $ map pan $ take n $ lines c
