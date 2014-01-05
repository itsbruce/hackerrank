import Control.Monad
import Text.Regex.PCRE

findIn :: [String] -> String -> Int
findIn xs y = let stem = take (length y - 2) y
                  y' = "\\b" ++ stem ++ "[sz]e\\b"
              in sum $ map (=~ y') xs

main = do
    n <- readLn
    xs <- replicateM n getLine
    t <- readLn
    c <- getContents
    let ys = take t $ lines c
    putStr $ unlines $ map (show . findIn xs) ys
