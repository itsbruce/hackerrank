import Control.Monad (replicateM)
import Data.Word

maxDraws :: Word32 -> Word32
maxDraws = succ

runTest = print =<< fmap maxDraws readLn

main = do
    t <- readLn
    replicateM t runTest
