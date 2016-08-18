{-# LANGUAGE ViewPatterns #-}
import qualified Data.Sequence as S

data Triple a = Once {top :: a} | Twice {top :: a} | Thrice {top :: a}
    deriving Show

promote d (Once x) = (id, Twice (x + d))
promote d (Twice x) = ((Thrice (x + d) S.<|), Twice (x + d))
promote d _ = error "Attempt to overpromote"

enqueue d x (S.viewl -> S.EmptyL) = S.singleton $ Once x
enqueue d x q@(S.viewl -> t S.:< ts) =
    let (prepend, suffix) = promote d t
        need = top t + d
    in  case compare need x of
            LT -> enqueue d x ts
            EQ -> prepend $ ts S.|> suffix
            GT -> q S.|> Once x

triples d xs = go S.empty xs
    where
        go _ [] = []
        go q (x:xs) = go' (enqueue d x q) xs
        go' (S.viewl -> (Thrice n) S.:< q) = (n :) . go q
        go' q = go q

readN n = fmap (take n . map read . words) getLine

main = do
    [n, d] <- readN 2
    xs <- readN n
    print $ length $ triples d xs
