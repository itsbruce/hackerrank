{-# LANGUAGE ViewPatterns #-}
import Control.Monad (when)
import qualified Data.Sequence as S
import Pipes
import qualified Pipes.Prelude as P

enqueue d x (S.viewl -> S.EmptyL) = S.singleton (x, 1)
enqueue d x q@(S.viewl -> s@(top, n) S.:< ss) =
    let need = top + d
    in  case compare need x of
            LT -> enqueue d x ss
            EQ -> ss S.|> (need, n + 1)
            GT -> q S.|> (x, 1)

triples d = go S.empty
    where
        go q = do
            x <- await
            let q' = enqueue d x q
            when (thrice x q') $ yield x
            go q'
        thrice x (S.viewr -> _ S.:> (_, n)) = n > 2
        thrice _ _ = False

countTriples d xs = head $ runEffect $ P.length $ each xs >-> triples d

readN n = fmap (take n . map read . words) getLine

main = do
    [n, d] <- readN 2
    xs <- readN n
    print $ countTriples d xs
