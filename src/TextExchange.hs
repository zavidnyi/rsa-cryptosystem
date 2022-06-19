module TextExchange where

import Data.Char ( ord, chr )
import KeyPairGeneration ( powMod )


encrypt :: Integer -> (Integer, Integer) -> Integer
encrypt m (e, n) = powMod m e n

decrypt :: Integer -> (Integer, Integer) -> Integer
decrypt c (d, n) = powMod c d n

encryptText :: String -> (Integer, Integer) -> Integer
encryptText s = encrypt (textToInt 0 s)

decryptText :: Integer -> (Integer, Integer) -> String
decryptText s key = show (intToText (decrypt s key))

textToInt :: Integer  -> [Char] -> Integer
textToInt _ [] = 0
textToInt exp (x:xs) = (fromIntegral (ord x) * 128^exp) + textToInt (exp+1) xs

intToText :: Integer -> [Char]
intToText 0 = []
intToText msgi = chr (fromIntegral (msgi `mod` 128)) : intToText (msgi `div` 128)