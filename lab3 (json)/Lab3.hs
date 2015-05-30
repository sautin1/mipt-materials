{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Directory
import System.Random
import Control.Monad
import Control.Monad.State
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Encoding
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Char as Char
import Network (withSocketsDo)

-- почтовый адрес
email = "sautin@phystech.edu"

data JSON = JNull
          | JBool Bool
          | JNumber Double
          | JString String
          | JObject [(String, JSON)]
          | JArray [JSON]

-- добавим сответствующие классы типов для JSON
instance Show JSON where
    show = stringify

data Token = OpenBrace | CloseBrace
           | OpenBracket | CloseBracket
           | Comma | Colon
           | Str String
           | Number Double
           | Null
           | Boolean Bool
        deriving (Show)

tokenize :: String -> [Token]
tokenize = tokenize' []
    where
        tokenize' acc [] = reverse acc
        tokenize' acc ('{': xs) = tokenize' (OpenBrace : acc) xs
        tokenize' acc ('}': xs) = tokenize' (CloseBrace : acc) xs
        tokenize' acc ('[': xs) = tokenize' (OpenBracket : acc) xs
        tokenize' acc (']': xs) = tokenize' (CloseBracket : acc) xs
        tokenize' acc (':': xs) = tokenize' (Colon : acc) xs
        tokenize' acc (',': xs) = tokenize' (Comma : acc) xs
        tokenize' acc ('n':'u':'l':'l': xs) = tokenize' (Null : acc) xs
        tokenize' acc ('t':'r':'u':'e': xs) = tokenize' (Boolean True : acc) xs
        tokenize' acc ('f':'a':'l':'s':'e': xs) = tokenize' (Boolean False : acc) xs
        tokenize' acc ('"': xs) = tokenize' (Str s : acc) t
            where
                (s, t) = parseString "" xs
                parseString acc ('\\' : '"' : t) = parseString (acc ++ "\"") t  -- "abc\"def" --- это экранированные кавычки
                parseString acc ('\\' : 'n' : t) = parseString (acc ++ "\n") t
                parseString acc ('"' : t)        = (reverse acc, t)             -- а это неэкранированные кавычки
                parseString acc (c : t)          = parseString (c : acc) t
        tokenize' acc (c : xs)
            | Char.isSpace c = tokenize' acc xs
            | Char.isDigit c || c == '-' = tokenize' (Number (read n :: Double) : acc) t
                where
                    (n, t) = parseNumber [c] xs
                    parseNumber acc [] = (reverse acc, [])
                    parseNumber acc t@(x : xs)
                        | Char.isSpace x                = (reverse acc, xs)
                        | x `elem` [')', ':', ',', ']'] = (reverse acc, t)
                        | otherwise                     = parseNumber (x : acc) xs

parse :: String -> (JSON, [Token])
parse s = parse' $ tokenize s
    where
        parse' (Null : ts)          = (JNull, ts)
        parse' (Str s : ts)         = (JString s, ts)
        parse' (Number x : ts)      = (JNumber x, ts)
        parse' (Boolean b : ts)     = (JBool b, ts)
        parse' (OpenBrace : ts)     = (JObject obj, t)
            where
                (obj, t) = parseObject [] ts
                parseObject acc (CloseBrace : tl)       = (reverse acc, tl)
                parseObject acc (Comma : tl)            = parseObject acc tl
                parseObject acc (Str key : Colon : tl)  = parseObject ((key, value) : acc) rest
                    where
                        (value, rest) = parse' tl
        parse' (OpenBracket : ts)   = (JArray arr, t)
            where
                (arr, t) = parseArray [] ts
                parseArray acc (CloseBracket : tl)  = (reverse acc, tl)
                parseArray acc (Comma : tl)         = parseArray acc tl
                parseArray acc tl                   = parseArray (value : acc) rest
                    where
                        (value, rest) = parse' tl

degreeJSON :: JSON -> Int
degreeJSON js@(JArray l)    = max (length l) $ maximum $ map degreeJSON l
degreeJSON js@(JObject l)   = max (length l) $ maximum $ map (degreeJSON . snd) l
degreeJSON _                = 0

lab3 :: JSON -> Int
lab3  = degreeJSON

stringify :: JSON -> String
stringify JNull         = "null"
stringify (JBool True)  = "true"
stringify (JBool False) = "false"
stringify (JNumber x)   = show x
stringify (JString s)   = "\"" ++ s ++ "\""
stringify (JArray l)    = "[" ++ arrStr ++ "]"
    where
        arrStr = intercalate ", " $ map stringify l
stringify (JObject l)   = "{ " ++ objStr ++ " }"
    where 
        objStr = intercalate "," $ map (\(key, val) -> "\"" ++ key ++ "\": " ++ stringify val) l

-- вариант с монадой IO
generateLength :: IO Int
generateLength = randomRIO (1, 5) :: IO Int

generateString :: IO String
generateString = do
    len <- generateLength
    replicateM len $ randomRIO ('a', 'z')

generateEntity :: Int -> IO JSON
generateEntity 0 = return JNull
generateEntity 1 = (randomIO :: IO Bool) >>= return . JBool
generateEntity 2 = (randomIO :: IO Double) >>= return . JNumber
generateEntity 3 = generateString >>= return . JString
generateEntity 4 = generateArray
generateEntity _ = generateObject

generateArray :: IO JSON
generateArray = do
    len <- generateLength
    types <- replicateM len $ randomRIO (0, 5)
    mapM generateEntity types >>= (return . JArray)

generateObject :: IO JSON
generateObject = do
    len     <- generateLength
    types   <- replicateM len $ randomRIO (0, 5)
    values  <- mapM generateEntity types
    keys    <- replicateM len $ generateString
    return $ JObject $ zip keys values
    
printRandomJSON :: IO ()
printRandomJSON = do
    json <- generateObject
    print json


 --чистый вариант с генератором, заключённым в состояние
 --мы храним в состоянии генератор, каждый раз используя
 --его, возвращаем в состояние новый

--type GeneratorState = State StdGen

--generateEntity :: Int -> GeneratorState JSON
--generateEntity x = do
--  case x of
--      0 -> 

--generate' :: GeneratorState JSON
--generate' = do
--  gen <- get
--  let (num, newGen) = randomR (1, maxObjectLength) gen :: (Int, StdGen)
--  let types = randomRs (0, 5) gen :: (Int, StdGen)
--  let json = case num of
--                           1 -> JObject [];
--                           2 -> JObject [("pure", JObject [])]
--  put newGen
--  return json

--generate :: IO JSON
--generate = evalState generate' (mkStdGen 0)

main :: IO()
main = withSocketsDo $ do
    dir <- getCurrentDirectory
    initReq <- parseUrl "http://mipt.eu01.aws.af.cm/lab3"
    handle <- openFile (dir ++ "/Lab3.hs") ReadMode
    hSetEncoding handle utf8_bom
    content <- hGetContents handle
    let req = urlEncodedBody [("email", email), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
    response <- withManager $ httpLbs req
    hClose handle
    L.putStrLn $ responseBody response