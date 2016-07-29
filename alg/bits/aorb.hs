import Control.Monad
import Data.Bits
import Data.List (foldl', mapAccumL, sort, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord
import Data.Word
import qualified Data.Vector.Unboxed as V

data Action a = Changed a | Unchanged a
    deriving Show

binMap :: M.Map Char [Word8]
binMap = M.fromList [
    ('0', [0,0,0,0]),
    ('1', [0,0,0,1]),
    ('2', [0,0,1,0]),
    ('3', [0,0,1,1]),
    ('4', [0,1,0,0]),
    ('5', [0,1,0,1]),
    ('6', [0,1,1,0]),
    ('7', [0,1,1,1]),
    ('8', [1,0,0,0]),
    ('9', [1,0,0,1]),
    ('A', [1,0,1,0]),
    ('B', [1,0,1,1]),
    ('C', [1,1,0,0]),
    ('D', [1,1,0,1]),
    ('E', [1,1,1,0]),
    ('F', [1,1,1,1])
    ]

hex2bin = concatMap char2bin
    where char2bin = (M.!) binMap

hexVec = V.fromList [
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
    ]

word2hex :: Word8 -> Char
word2hex = (V.unsafeIndex hexVec) . fromIntegral

bin2word :: [Word8] -> Word8
bin2word = foldl' shiftIn 0
    where
        shiftIn 0 b = b
        shiftIn a b = b .|. (shift a 1)

bin2hex = (map (word2hex . bin2word)) . by4
    where
        by4 [] = []
        by4 (a:b:c:d:rest) = [[a,b,c,d]] ++ (by4 rest)

compareBins :: [Word8] -> [Word8] -> Ordering
compareBins = ((go . (dropWhile (== EQ))) .) . (zipWith compare)
    where
        go [] = EQ
        go (x : _) = x

align :: (Word8 , Word8 , Word8) -> [Action (Word8, Word8, Word8)]
align t@(a, b, c)
    | a .|. b == c = [Unchanged t]
    | c == 0 = [Changed (0,0,0)]
    | otherwise = [Changed (0, 1, 1), Changed (1, 0, 1)]

mapChanges = mapM align

unChange k = sequence . snd . (mapAccumL extract k)
    where
        extract n (Unchanged x) = (n, Just x)
        extract 0 (Changed x) = (0, Nothing)
        extract n (Changed x) = (n - 1, Just x)

mapBack k = sequence . map (unChange k)

takeAB = twoFromThree . unzip3
    where
        twoFromThree (a, b, c) = (a, b)

zip3bin a b c = zip3 (hex2bin a) (hex2bin b) (hex2bin c)

alignABC k a b c = fmap ((\(a, b) -> (bin2hex a, bin2hex b)) . head . sort . (map takeAB)) $
    mapBack k $ mapChanges $ zip3bin a b c

runQ = do
    k <- readLn
    a <- getLine
    b <- getLine
    c <- getLine
    return $ alignABC k a b c

printAB Nothing = print (-1)
printAB (Just (a, b)) = printTrim a >> printTrim b
    where printTrim = putStrLn . (dropWhile (== '0'))

main = do
    q <- readLn
    qs <- replicateM q runQ
    mapM_ printAB qs

