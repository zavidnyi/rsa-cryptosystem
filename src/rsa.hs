import System.IO ()
import System.Environment ( getArgs )
import KeyPairGeneration ( getRSAKeyPairs )
import TextExchange ()
import Numeric (showHex)
import Data.Char ( toUpper ) 
import Text.Read ( readMaybe )



helpMSG :: String
helpMSG =
    "RSA crytosystem\n" ++ 
    "To generate RSA key pair use this command: rsa -gen [key size in bits, defaults to 512]"

main = do
    args <- getArgs
    str <- parseArgs args
    putStr str

parseArgs :: [String] -> IO String
parseArgs args
    | null args = return "No arguments given, aborting..."
    | head args == "-gen" = if length args < 2 then genKeys (args ++ ["512"]) else genKeys args
    | otherwise = return ("WRONG ARGUMENTS\n" ++ helpMSG)

getInteger :: String -> Integer
getInteger str = do 
    let mint = readMaybe str :: Maybe Integer
    case mint of
        Nothing -> -1
        Just int -> int

intToHex :: Integer -> String
intToHex n = map toUpper (showHex n "")

genKeys :: [String] -> IO String
genKeys args = do
    let inp = getInteger (args !! 1)
    if inp <= 0 then return "Invalid key-size\n" else do
    let keySize = inp `div` 2
    ((e, n), (d, _)) <- getRSAKeyPairs (2^(keySize-1))  (2^keySize) 10
    writeFile "pub.key" (intToHex e ++ "\0" ++ intToHex n)
    writeFile "priv.key" (intToHex d ++ "\0" ++ intToHex n)
    return ("\x1b[31m" ++ "PUBLIC KEY\n" ++ "\x1b[37m" ++ intToHex e ++ "\0" ++ intToHex n ++ "\x1b[31m" ++
            "\nPRIVATE KEY\n" ++ "\x1b[37m" ++ intToHex d ++ "\0" ++ intToHex n ++ "\0\n")
