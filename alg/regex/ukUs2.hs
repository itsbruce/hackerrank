import Control.Monad
import Text.Regex.PCRE

findIn :: [String] -> String -> Int
findIn xs y = let (p,_,s) = y =~ "our" :: (String,String,String)
                  y' = "\\b" ++ p ++ "ou?r" ++ s ++ "\\b"
              in sum $ map (=~ y') xs

main = do
    n <- readLn
    xs <- replicateM n getLine
    t <- readLn
    c <- getContents
    let ys = take t $ lines c
    putStr $ unlines $ map (show . findIn xs) ys
