import Data.Array
import Text.Regex.PCRE

langs = ["C","CPP","JAVA","PYTHON","PERL","PHP","RUBY","CSHARP","HASKELL","CLOJURE","BASH","SCALA","ERLANG","CLISP","LUA","BRAINFUCK","JAVASCRIPT","GO","D","OCAML","R","PASCAL","SBCL","DART","GROOVY","OBJECTIVEC"]

validity :: String -> String
validity s = let p = "^\\d{5}\\s+(\\w*)"
                 m = s =~ p :: MatchArray
                 sub = (\(i, l) -> take l (drop i s)) (m ! 1)
             in if elem sub langs then "VALID" else "INVALID"

main = do
    n <- readLn
    c <- getContents
    putStr $ unlines $ map validity $ take n $ lines c
