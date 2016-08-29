{-# LANGUAGE ViewPatterns #-}
import Control.Monad (replicateM)
import Data.Foldable (toList)
import Data.Sequence (
        (|>),(><),ViewR(..),
        Seq,fromList,singleton,spanl,splitAt,viewr
    )
import Prelude hiding (splitAt)


gsort (viewr -> EmptyR) _ = Nothing
gsort (viewr -> p :> x) s@(viewr -> s' :> y)
    | x >= y = gsort p $ s |> x
    | otherwise = result p x s

result p y s =
    let (lower, higher) = spanl (<= y) s
        (x, rest) = splitAt 1 higher
    in  Just $ p >< x >< (lower |> y) >< rest

greater (viewr -> EmptyR) = Nothing
greater (viewr -> xs :> x) = gsort xs $ singleton x

greaterIO = fmap (greater . fromList) getLine

printResult Nothing = putStrLn "no answer"
printResult (Just x) = putStrLn $ toList x

main = do
    t <- readLn
    replicateM t (printResult =<< greaterIO)
