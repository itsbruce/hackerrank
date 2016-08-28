{-# LANGUAGE ViewPatterns #-}
import Control.Monad (liftM2,msum,replicateM)
import Data.Foldable (toList)
import Data.Sequence (
        (<|),(><),ViewR(..),
        Seq,spanr,empty,fromList,length,sort,splitAt,viewr
    )
import Prelude hiding (length,reverse,sort,splitAt)

data Stages a =
    Test {value :: a, prefix :: Seq a, suffix :: Seq a} |
    Tested {value :: a, part1 :: Seq a, part2 :: Seq a, suffix :: Seq a} |
    Found { value :: a, prefix :: Seq a, suffix :: Seq a }
        deriving Show

test (Test v p s) =
    let (p1, p2) = spanr (>= v) p
    in  case (viewr p) of
        EmptyR -> Nothing
        _ -> measure $ Tested v p1 p2 s

measure (Tested v p1 p2 s) =
    case (viewr p1, viewr p2) of
        (ps :> p, EmptyR) -> test $ Test p ps (v <| s)
        (_, (xs :> x)) -> result $ Found v xs (x <| p1 >< s)
        _ -> Nothing

result (Found v p s) = Just $ p >< (v <| sort s)

greaterPart xs = go xs
    where
        go (viewr -> ys :> y) = test $ Test y ys empty

greater (viewr -> EmptyR) = Nothing
greater (viewr -> (viewr -> EmptyR) :> _) = Nothing
greater xs = 
    let l = length xs
        splits = fmap (flip splitAt xs) [l-1,l-2..0]
        tries = map (fmap greaterPart) splits
        unsplit (p, t) = liftM2 (><) (Just p) t
        unsplits = fmap unsplit tries
    in  msum unsplits

greaterIO = fmap (greater . fromList) getLine

printResult Nothing = putStrLn "no answer"
printResult (Just x) = putStrLn $ toList x

main = do
    t <- readLn
    replicateM t (printResult =<< greaterIO)
