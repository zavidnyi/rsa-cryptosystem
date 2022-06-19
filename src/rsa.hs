import System.IO ()
import System.Environment ( getArgs )
import KeyPairGeneration ( getRSAKeyPairs )
import TextExchange ()
import Numeric (showHex)
import Data.Char ( toUpper ) 



helpMSG =
    "RSA crytosystem\n" ++ 
    "To generate RSA key pair use this command: rsa -gen [optional: key size in bits]"

main = do
    args <- getArgs
    str <- parseArgs args
    putStr str

parseArgs :: [[Char]] -> IO String
parseArgs args
    | null args = return "No arguments given, aborting..."
    | head args == "-gen" = genKeys args
    | otherwise = return ("WRONG ARGUMENTS\n" ++ helpMSG)

getInteger :: String -> IO Integer
getInteger str = do return (read str)

intToHex :: Integer -> String
intToHex n = map toUpper (showHex n "")

genKeys args = do
    inp <- getInteger (args !! 1)
    let keySize = inp `div` 2
    ((e, n), (d, _)) <- getRSAKeyPairs (2^(keySize-1))  (2^keySize) 10
    writeFile "pub.key" (intToHex e ++ "\0" ++ intToHex n)
    writeFile "priv.key" (intToHex d ++ "\0" ++ intToHex n)
    return ("\x1b[31m" ++ "PUBLIC KEY\n" ++ "\x1b[37m" ++ intToHex e ++ "\0" ++ intToHex n ++ "\x1b[31m" ++
            "\nPRIVATE KEY\n" ++ "\x1b[37m" ++ intToHex d ++ "\0" ++ intToHex n ++ "\0\n")
