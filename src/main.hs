import System.Random
import Data.List

import System.IO


randomInteger :: RandomGen g => Integer -> Integer -> g -> ( Integer, g) 
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

getRandomPrime :: RandomGen g => Integer -> Integer -> Integer -> g -> Integer
getRandomPrime l h k g =
    let
        (n, g')= randomInteger l h g
        (d, r) = rabinMillerForm n
    in if rabinMillerTest n d r k g' then n else getRandomPrime l h k g'

rabinMillerForm :: Integer -> (Integer, Integer)
rabinMillerForm n = (d, toInteger r) where
    factorList = iterate (`div` 2) (pred n)
    r = length $ takeWhile even factorList
    d = factorList !! r

rabinMillerTest :: RandomGen g => Integer -> Integer -> Integer -> Integer -> g -> Bool
rabinMillerTest n d r k g
    | k <= 0 = True
    | otherwise =
        let
            (a, g') = randomInteger 2 (n-2) g
            x = powMod a d n
        in
            if x == 1 || x == n-1 then rabinMillerTest n d r (pred k) g' else rabinMillerInnerLoop n r x (pred k) g'

rabinMillerInnerLoop :: RandomGen g => Integer -> Integer -> Integer -> Integer -> g -> Bool
rabinMillerInnerLoop n r x k g
    | r <= 0 = False
    | otherwise = 
        let
            x' = powMod x 2 n
            (d, r') = rabinMillerForm n
        in if x' == n-1 then rabinMillerTest n d r' k g else rabinMillerInnerLoop n (pred r) x' k g

main = do
        randGen <- getStdGen
        putStr "Generating key, please wait...\n"
        getRandomPrime (2^11) (2^12) 4 randGen
        putStr "Success!\n"
