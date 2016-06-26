import Control.Monad (liftM)
data CharCount = Zero |  Count Char Int

express :: CharCount -> [Char]
express Zero = []
express (Count x 1) = [x]
express (Count x n) = x : (show n)

compress :: [Char] -> [Char]
compress =  let tally x counts = case counts of
                    [] -> [Count x 1]
                    Zero : cs -> (Count x 1) : cs
                    (Count y n) : cs -> if (x == y)
                        then (Count y (n + 1)) : cs
                        else (Count x 1) : counts
            in  (concatMap express) . (foldr tally [])

main = (liftM compress getLine) >>= putStrLn
