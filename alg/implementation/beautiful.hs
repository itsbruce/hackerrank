{-# LANGUAGE ViewPatterns #-}
import qualified Data.Sequence as S

enqueue d x (S.viewl -> S.EmptyL) = S.singleton (x, 1)
enqueue d x q@(S.viewl -> (top, n) S.:< ss) =
    let need = top + d
    in  case compare need x of
            LT -> enqueue d x ss
            EQ -> ss S.|> (need, n + 1)
            GT -> q S.|> (x, 1)

intervals d = go S.empty
    where
        go _ [] = []
        go q (x:xs) = go' (enqueue d x q) xs
        go' q@(S.viewr -> _ S.:> x) = (x :) . go q

triples = filter ((> 2) . snd)

countTriples d = length . triples . intervals d

readN n = fmap (take n . map read . words) getLine

main = do
    [n, d] <- readN 2
    xs <- readN n
    print $ countTriples d xs
