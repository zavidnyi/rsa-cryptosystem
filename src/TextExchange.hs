module TextExchange where

import Data.Char ( ord, chr )
import KeyPairGeneration ( powMod )
import GhcPlugins (text)


encrypt :: (Integer, Integer) -> Integer -> Integer
encrypt (exp, n) m = powMod m exp n

decrypt :: (Integer, Integer) -> Integer -> Integer
decrypt (exp, n) m = powMod m exp n

encryptText :: String -> (Integer, Integer) -> String
encryptText s key = show text
    where
        text = map (encrypt key . textToInt 0) (getBlocks 64 s)

decryptText :: String -> (Integer, Integer) -> String
decryptText s key = text
    where
        text = concatMap (intToText . decrypt key) (read s :: [Integer])

textToInt :: Integer  -> [Char] -> Integer
textToInt _ [] = 0
textToInt exp (x:xs) = (fromIntegral (ord x) * 128^exp) + textToInt (exp+1) xs

intToText :: Integer -> [Char]
intToText 0 = []
intToText msgi = chr (fromIntegral (msgi `mod` 128)) : intToText (msgi `div` 128)

getBlocks :: Int -> [Char] -> [[Char]]
getBlocks _ [] = []
getBlocks len raw = let (front, rest) = splitAt len raw in front : getBlocks len rest