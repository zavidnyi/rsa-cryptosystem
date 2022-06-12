import System.Random
import Data.List

randomInteger l h = randomR (l, h)

getRandomPrime l h gen =
    let
        (n, gen') = randomInteger l h gen
    in if nub (map (millerRabinPrimality n) (getDetBases n)) == [True] then n else getRandomPrime l h gen'


-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        

-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = 
        error $ "millerRabinPrimality: a out of range (" 
              ++ show a ++ " for "++ show n ++ ")" 
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs

-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a

-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

getDetBases :: Integer -> [Integer]
getDetBases n
    | n < 2047 = [2]
    | n < 1373653 = [2,3]
    | n < 9080191 = [31, 73]
    | n < 25326001 = [2,3,5]
    | n < 3215031751 = [2,3,5,7]
    | n < 4759123141 = [2,7,61]
    | n < 1122004669633 = [2,13,23,1662803]
    | n < 2152302898747 = [2,3,5,7,11]
    | n < 3474749660383 = [2,3,5,7,11,13]
    | n < 341550071728321 = [2,3,5,7,11,13,17]
    | n < 3825123056546413051 = [2,3,5,7,11,13,17,19,23]
    | n < 18446744073709551616 = [2,3,5,7,11,13,17,19,23,31,37]
    | n < 318665857834031151167461 = [2,3,5,7,11,13,17,19,23,31,37]
    | n < 3317044064679887385961981 = [2,3,5,7,11,13,17,19,23,31,37,41]
    | otherwise = []
