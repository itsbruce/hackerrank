import Data.Maybe
import Control.Applicative
import Control.Monad
import Text.Regex.PCRE

isip :: String -> String
isip = fromMaybe "Neither" . msum . (<*>) [isv4, isv6] . pure
    where mif p v s = if s =~ p then Just v else Nothing
          isv4 = mif "^[12]?\\d{1,2}(?:\\.[12]?\\d{1,2}){1,3}$" "IPv4"
          isv6 = mif "(?i)(?:(?:[\\dabcdef]{0,4}:){1,7}[\\dabcdef]{1,4}|::1?)$" "IPv6"

main = do
    n <- readLn
    c <- getContents
    putStr $ unlines $ map isip $ take n $ lines c
