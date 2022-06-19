module KeyPairGeneration where

import Data.List
import System.Random (randomRIO)

--------START: Random prime generation--------

-- Generates random interger in range [l,h] using global random number generator
randomInteger :: Integer -> Integer -> IO Integer
randomInteger l h = randomRIO (l, h)

-- Taken from documentation:
-- https://hackage.haskell.org/package/arithmoi-0.12.0.1/docs/src/Math.NumberTheory.Powers.Modular.html#powMod
powMod :: (Integral a, Integral b) => a -> b -> a -> a
powMod x y m
  | m <= 0 = error "powModInt: non-positive modulo"
  | y < 0 = error "powModInt: negative exponent"
  | otherwise = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc =
      f
        (b * b `rem` m)
        (e `quot` 2)
        (if odd e then b * acc `rem` m else acc)

-- For prime number generation Rabin Miller test is used:
-- https://en.wikipedia.org/wiki/Miller–Rabin_primality_test

getRandomPrime :: Integer -> Integer -> Integer -> IO Integer
getRandomPrime l h k = do
  n <- randomInteger l h
  let (d, r) = rabinMillerForm n
  isPrime <- rabinMillerTest n d r k
  if isPrime then return n else getRandomPrime l h k

-- Splits number n into 2^r*d + 1
rabinMillerForm :: Integer -> (Integer, Integer)
rabinMillerForm n = (d, toInteger r)
  where
    factorList = iterate (`div` 2) (pred n)
    r = length $ takeWhile even factorList
    d = factorList !! r

-- Corresponds to the outer loop of Miller rabin test mentioned above ^
rabinMillerTest :: Integer -> Integer -> Integer -> Integer -> IO Bool
rabinMillerTest n d r k
  | k <= 0 = return True
  | otherwise =
    do
      a <- randomInteger 2 (n -2)
      let x = powMod a d n
      if x == 1 || x == n -1 then rabinMillerTest n d r (pred k) else rabinMillerInnerLoop n r x (pred k)


-- Corresponds to the INNER loop of Miller rabin test mentioned above ^
rabinMillerInnerLoop :: Integer -> Integer -> Integer -> Integer -> IO Bool
rabinMillerInnerLoop n r x k
  | r <= 0 = return False
  | otherwise =
    let x' = powMod x 2 n
        (d, r') = rabinMillerForm n
     in if x' == n -1 then rabinMillerTest n d r' k else rabinMillerInnerLoop n (pred r) x' k

--------END: Random prime generation--------

--------START: Key pair generation--------

-- Implementation of Extended euclidian algorithm
egcd :: Integer -> Integer -> (Integer, Integer)
egcd a b = egcdHelp (a, b) (1, 0) (0, 1)
  where
    egcdHelp (r', r) (s', s) (t', t)
      | r == 0 = (s', t')
      | otherwise = egcdHelp (r, r' - q * r) (s, s' - q * s) (t, t' - q * t)
      where
        q = quot r' r


getPrivateExponent :: Integer -> Integer -> Integer -> IO Integer
getPrivateExponent e p q =
  return (powMod (fst (egcd e ((p - 1) * (q - 1)))) 1 ((p - 1) * (q - 1)))


-- Generates RSA key pairs
-- For public exponent we use common e = 2^16 + 1 = 65,537
-- Private exponent is generated as it should i.e.  d = e^(-1)(mod ((p-1) * (q-1)))
getRSAKeyPairs :: Integer -> Integer -> Integer -> IO ((Integer, Integer), (Integer, Integer))
getRSAKeyPairs m n k =
  do
    p <- getRandomPrime m n k
    q <- getRandomPrime m n k
    let e = 65537
    d <- getPrivateExponent e p q
    return ((e, p * q), (d, p * q))

--------END: Key pair generation-------