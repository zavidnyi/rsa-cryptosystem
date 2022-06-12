import System.Random
import Data.List

import System.IO


randomInteger :: Integer -> Integer-> IO Integer
randomInteger l h = randomRIO (l, h)

-- Taken from documentation
powMod :: (Integral a, Integral b) => a -> b -> a -> a
powMod x y m
  | m <= 0    = error "powModInt: non-positive modulo"
  | y <  0    = error "powModInt: negative exponent"
  | otherwise = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc = f (b * b `rem` m) (e `quot` 2)
      (if odd e then b * acc `rem` m else acc)

getRandomPrime :: Integer -> Integer -> Integer-> IO Integer
getRandomPrime l h k  =
    do
        n <- randomInteger l h 
        let (d, r) = rabinMillerForm n 
        isPrime <- rabinMillerTest n d r k
        if isPrime then return n else getRandomPrime l h k

rabinMillerForm :: Integer -> (Integer, Integer)
rabinMillerForm n = (d, toInteger r) where
    factorList = iterate (`div` 2) (pred n)
    r = length $ takeWhile even factorList
    d = factorList !! r

rabinMillerTest :: Integer -> Integer -> Integer -> Integer -> IO Bool
rabinMillerTest n d r k 
    | k <= 0 = return True
    | otherwise =
        do
            a <- randomInteger 2 (n-2)
            let
                x = powMod a d n
            if x == 1 || x == n-1 then rabinMillerTest n d r (pred k) else rabinMillerInnerLoop n r x (pred k)

rabinMillerInnerLoop :: Integer -> Integer -> Integer -> Integer -> IO Bool
rabinMillerInnerLoop n r x k
    | r <= 0 = return False
    | otherwise = 
        let
            x' = powMod x 2 n
            (d, r') = rabinMillerForm n
        in if x' == n-1 then rabinMillerTest n d r' k else rabinMillerInnerLoop n (pred r) x' k


egcd a b = egcdHelp (a, b) (1, 0) (0, 1)
egcdHelp (r', r) (s', s) (t', t) 
    | r == 0 = (s', t')
    | otherwise = egcdHelp (r, r' - q*r) (s, s' - q*s) (t, t' - q*t) where q = quot r'  r

getPrivateExponent :: Integer -> Integer -> Integer -> IO Integer
getPrivateExponent e p q = 
        return (powMod (fst (egcd e ((p - 1) * (q - 1)))) 1 ((p - 1) * (q - 1)))

getRSAKeyPairs :: Integer -> Integer -> Integer -> IO ((Integer, Integer), (Integer, Integer))
getRSAKeyPairs m n k =
    do
        p <- getRandomPrime m n k
        q <- getRandomPrime m n k
        let e = 65537
        d <- getPrivateExponent e p q
        return ((e, p * q), (d, p * q))

main = do
        getRSAKeyPairs (2^11) (2^12) 4


encrypt :: Integer -> (Integer, Integer) -> Integer
encrypt m (e, n) = powMod m e n

-- Decrypts a message c using private key pair prk=(d, n)
decrypt :: Integer -> (Integer, Integer) -> Integer
decrypt c (d, n) = powMod c d n