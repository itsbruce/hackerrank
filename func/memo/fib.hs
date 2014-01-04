fibs = 0 : 1 : map f [2..]
  where f n = (fibs !! (n - 2)) + (fibs !! (n - 1))

mod1087 = flip mod 100000007

main = do
  n <- readLn :: IO Int
  c <- getContents
  let xs = map read $ lines c
  putStr $ unlines $ map (show . mod1087 . ((!!) fibs)) xs
