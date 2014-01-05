import Data.Array
import Text.Printf
import Text.Regex.PCRE

parse :: String -> String
parse s = printf str (sub 1) (sub 2) (sub 3)
    where str = "CountryCode=%s,LocalAreaCode=%s,Number=%s"
          m = s =~ "^(\\d{1,3})[ -](\\d{1,3})[ -](\\d{4,10})$" :: MatchArray
          sub = (\(i, l) -> take l (drop i s)) . (!) m

main = do
    n <- readLn
    c <- getContents
    let xs = take n $ lines c
    putStr $ unlines $ map parse xs
