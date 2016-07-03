import Data.Monoid
import Data.Foldable (foldMap)
import qualified Data.Vector as V
-- import Control.Monad.Trans.State.Strict
import Control.Monad.State

newtype MinMult a = MinMult { getMinMult :: a }
    deriving (Eq, Ord, Read, Show)

instance (Integral a) => Monoid (MinMult a) where
    mempty = MinMult 1
    mappend (MinMult x) (MinMult y) = MinMult (lcm x y)

type AppState a = State (V.Vector Int) a

lcmState :: State (V.Vector Int) Int
lcmState = do
    v <- get
    return $ getMinMult $ foldMap MinMult v

updateState :: (Int, Int) -> State (V.Vector Int) ()
updateState x = modify updateV
    where updateV = flip V.update (V.singleton x)
            
