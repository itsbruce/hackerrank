
type Position = (Int, Int)

board :: Int -> [Position]
board n = [(r, c) | r <- range, c <- range]
    where range = [1..n]

safe :: Position -> [Position] -> [Position]
safe (r, c) = filter (\p -> (notR p) && (notC p) && (notD p) && (notK p))
    where notR = (/= r) . fst
          notC = (/= c) . snd
          notD (r', c') = abs (r' - r) /= abs (c' - c)
          notK (r', c') = notElem (abs (r' - r), abs (c' - c)) [(1, 2), (2, 1)]

place :: Int -> [Position] -> [Position]
place _ [] = []
place 0 ps = ps
place n (p:ps) = place (n - 1) (safe p ps)
