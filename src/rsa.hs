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
parseArgs args = case args of
    [] -> return "No arguments given, aborting...\n To see help run with `-h` flag"
    ["-h"] -> return helpMSG
    ["-gen"] -> genKeys 512
    ["-gen", size] -> genKeys (getInteger size)
    ["-encrypt", keyFile, textFile] -> encryptCMD keyFile textFile
    ["-decrypt", keyFile, textFile] -> decryptCMD keyFile textFile
    _ -> return ("WRONG ARGUMENTS\n" ++ helpMSG)

getInteger :: String -> Integer
getInteger str = fromMaybe (-1) (readMaybe str :: Maybe Integer)

intToHex :: Integer -> String
intToHex n = map toUpper (showHex n "")

genKeys :: Integer -> IO String
genKeys size = do
    if size <= 0 then return "Invalid key-size\n" else do
    let keySize = size `div` 2
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


encryptCMD :: String -> String -> IO String
encryptCMD keyFile textFile = do
    key <- readFile keyFile
    text <- readFile textFile
    case map (fst . head . readHex ) (split key '\0') of
        [exp, n] -> return (encryptText text (exp, n))
        _ -> return "WRONG KEY FORMAT"

decryptCMD :: String -> String -> IO String
decryptCMD keyFile textFile = do
    key <- readFile keyFile
    text <- readFile textFile
    case map (fst . head . readHex ) (split key '\0') of
        [exp, n] -> return (decryptText text (exp, n))
        _ -> return "WRONG KEY FORMAT"