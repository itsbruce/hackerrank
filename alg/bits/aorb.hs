import Control.Monad
import Data.Bits
import Data.Foldable (foldMap)
import Data.List (foldl', mapAccumL)
import qualified Data.Map.Strict as M
import Data.Ord
import Data.Word
import qualified Data.Vector.Unboxed as V

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

hex2bin = foldMap char2bin
    where char2bin = (M.!) binMap

hexVec = V.fromList [
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
    ]

word2hex :: Word8 -> Char
word2hex = V.unsafeIndex hexVec . fromIntegral

bin2word :: [Word8] -> Word8
bin2word = foldl' shiftIn 0
    where
        shiftIn 0 b = b
        shiftIn a b = b .|. shift a 1

bin2hex = map (word2hex . bin2word) . by4
    where
        by4 [] = []
        by4 (a:b:c:d:rest) = [[a,b,c,d]] ++ (by4 rest)

compareBins :: [Word8] -> [Word8] -> Ordering
compareBins = ((go . (dropWhile (== EQ))) .) . zipWith compare
    where
        go [] = EQ
        go (x : _) = x

noChoice (1,0,0) = [(,) 1 (0,0,0)]
noChoice (0,1,0) = [(,) 1 (0,0,0)]
noChoice (1,1,0) = [(,) 2 (0,0,0)]
noChoice t = [(,) 0 t]

choice (0,0,1) = [(,) 1 (0,1,1), (,) 1 (1,0,1)]
choice t@(1,0,1) = [(,) 2 (0,1,1), (,) 0 t]
choice t@(1,1,1) = [(,) 1 (0,1,1), (,) 1 (1,0,1), (,) 0 t]
choice t = [(,) 0 t]

unChange k = fmap sequence . mapAccumL extract k

extract n (0, x) = (n, [x])
extract 0 (_, _) = (0, [])
extract 1 (2, _) = (0, [])
extract n (c, x) = (n - c, [x])

align f _ [] = [[]]
align f n (x:rest) = do
    (c, t) <- f x
    guard (n - c >= 0)
    ts <- align f (n - c) rest
    return $ [t] ++ ts

alignChoice k = align choice k

safeFirst [] = Nothing
safeFirst ([] : rest) = safeFirst rest
safeFirst (x : _) = Just x

zipABC a b c =
    let xs = [a,b,c]
        n = foldr max 0 $ map length xs
        pad = reverse . take n . (++ repeat '0') . reverse
        [a', b', c'] = map (hex2bin . pad) xs
    in  zip3 a' b' c'

alignABC k a b c =
    let zipped = zipABC a b c
        (k', noChoices) = fmap safeFirst $ unChange k $ foldMap noChoice zipped
        align = join $ fmap (safeFirst . alignChoice k') noChoices
        takeAB = (\(x, y, z) -> (x, y)) . unzip3
        ab2hex = fmap ((\(a, b) -> (bin2hex a, bin2hex b)) . takeAB)
    in ab2hex align -- zipped

runQ = do
    k <- readLn
    a <- getLine
    b <- getLine
    c <- getLine
    return $ alignABC k a b c

printAB Nothing = print (-1)
printAB (Just (a, b)) = printTrim a >> printTrim b
    where printTrim = putStrLn . dropWhile (== '0')

main = do
    q <- readLn
    qs <- replicateM q runQ
    mapM_ printAB qs
