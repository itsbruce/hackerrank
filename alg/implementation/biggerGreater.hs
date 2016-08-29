{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
import Control.Monad (replicateM)
import Data.Foldable (toList)
import qualified Data.Sequence as S (ViewR(..),viewr)
import Data.Sequence (
        (|>),(><),
        Seq,fromList,singleton,spanl,splitAt
    )
import Prelude hiding (splitAt)

pattern Empty  <- (S.viewr -> S.EmptyR)
pattern xs :> x <- (S.viewr -> xs S.:> x)

gsort (Empty) _ = Nothing
gsort (p :> x) s@(_ :> y)
    | x >= y = gsort p $ s |> x
    | otherwise = result p x s

result p y s =
    let (lower, higher) = spanl (<= y) s
        (x, rest) = splitAt 1 higher
    in  Just $ p >< x >< (lower |> y) >< rest

greater (Empty) = Nothing
greater (xs :> x) = gsort xs $ singleton x

greaterIO = fmap (greater . fromList) getLine

printResult Nothing = putStrLn "no answer"
printResult (Just x) = putStrLn $ toList x

main = do
    t <- readLn
    replicateM t (printResult =<< greaterIO)
