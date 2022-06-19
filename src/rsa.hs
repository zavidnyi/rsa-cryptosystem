import System.IO ()
import System.Environment ( getArgs )
import KeyPairGeneration ( getRSAKeyPairs )
import TextExchange (encryptText, decryptText)
import Numeric (showHex, readHex)
import Data.Char ( toUpper )
import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe )
import Data.Text ( splitOn )


main :: IO ()
main = do
    args <- getArgs
    str <- parseArgs args
    putStr str


helpMSG :: String
helpMSG =
    "RSA crytosystem\n" ++
    "To display this message use: rsa -h\n" ++
    "To generate RSA key pair use this command: rsa -gen [key size in bits, defaults to 512]\n" ++
    "To encrypt ASCII text use this command: rsa -encrypt [path to key] [path to text]\n" ++
    "To decrypt ASCII text use this command: rsa -decrypt [path to key] [path to text]\n"

parseArgs :: [String] -> IO String
parseArgs args
    | null args = return "No arguments given, aborting...\n To see help run with `-h` flag"
    | head args == "-h" = return helpMSG
    | head args == "-gen" = if length args < 2 then genKeys (args ++ ["512"]) else genKeys args
    | head args == "-encrypt" = if length args < 3 then return ("WRONG ARGUMENTS\n" ++ helpMSG) else encryptCMD args
    | head args == "-decrypt" = if length args < 3 then return ("WRONG ARGUMENTS\n" ++ helpMSG) else decryptCMD args
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

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim


encryptCMD :: [String] -> IO String
encryptCMD args = do
    key <- readFile (args !! 1)
    text <- readFile (args !! 2)
    let keys = map (fst . head . readHex ) (split key '\0')
    if length keys /= 2 then return "WRONG KEY FORMAT" else do
    let [exp, n] = keys
    return (encryptText text (exp, n))

decryptCMD :: [String] -> IO String
decryptCMD args = do
    key <- readFile (args !! 1)
    text <- readFile (args !! 2)
    let keys = map (fst . head . readHex ) (split key '\0')
    if length keys /= 2 then return "WRONG KEY FORMAT" else do
    let [exp, n] = keys
    return (decryptText text (exp, n))