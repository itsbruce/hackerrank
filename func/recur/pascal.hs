triangle = [1] : [1,1] : map f [1..]
  where f n = let row = triangle !! n
                  newRow= foldr ((:) . (\(x,y) -> x + y)) [1] $ zip row $ tail row
              in  1 : newRow
              
showRow :: [Int] -> [Char]
showRow = unwords . map show
              
main = do
  rows <- readLn
  putStr $ unlines $ map showRow $ take rows triangle
