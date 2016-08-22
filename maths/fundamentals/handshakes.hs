import Control.Monad (replicateM)

handshakes 0 = 0
handshakes 1 = 0
handshakes 2 = 1
handshakes n = sum [n-1,n-2..1]

runShake = print =<< fmap handshakes readLn

main = do
    n <- readLn
    replicateM n runShake
