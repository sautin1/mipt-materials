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

email = "sautin@phystech.edu"


-- Data structures
data JSON = JNull
          | JBool Bool
          | JNumber Double
          | JString String
          | JObject [(String, JSON)]
          | JArray [JSON]

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

-- Read JSON from string and write it to string
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

-- Tree function
degreeJSON :: JSON -> Int
degreeJSON js@(JArray l)
    | null l    = length l
    | otherwise = max (length l) $ maximum $ map degreeJSON l
degreeJSON js@(JObject l)
    | null l    = length l
    | otherwise = max (length l) $ maximum $ map (degreeJSON . snd) l
degreeJSON _    = 0

lab3 :: JSON -> Int
lab3  = degreeJSON

-- Random JSON Object
-- IO monad option
generateLength :: IO Int
generateLength = randomRIO (0, 5) :: IO Int

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

-- Tests
type TestCase = (String, Int)
tester :: TestCase -> IO ()
tester (str, corRes) = do
    let (json, tail) = parse str
    let res = degreeJSON json
    if ((null tail) && (res == corRes)) then putStrLn "Test passed!"
                                        else putStrLn "Test failed!"

-- example from json.org/example
test1 :: TestCase
test1 = ("{\"glossary\": {\"title\": \"example glossary\", \"GlossDiv\": {\"title\": \"S\",\"GlossList\": {\"GlossEntry\": { \"ID\": \"SGML\", \"SortAs\": \"SGML\",\"GlossTerm\": \"Standard Generalized Markup Language\", \"Acronym\": \"SGML\", \"Abbrev\": \"ISO 8879:1986\", \"GlossDef\": { \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\", \"GlossSeeAlso\": [\"GML\", \"XML\"] }, \"GlossSee\": \"markup\"}}}}}"
        , 7)

-- example from json.org/example
test2 :: TestCase
test2 = ("{\"menu\": {\"header\": \"SVG Viewer\",\"items\": [{\"id\": \"Open\"},{\"id\": \"OpenNew\", \"label\": \"Open New\"},null,{\"id\": \"ZoomIn\", \"label\": \"Zoom In\"},{\"id\": \"ZoomOut\", \"label\": \"Zoom Out\"},{\"id\": \"OriginalView\", \"label\": \"Original View\"},null,{\"id\": \"Quality\"},{\"id\": \"Pause\"},{\"id\": \"Mute\"},null,{\"id\": \"Find\", \"label\": \"Find...\"},{\"id\": \"FindAgain\", \"label\": \"Find Again\"},{\"id\": \"Copy\"},{\"id\": \"CopyAgain\", \"label\": \"Copy Again\"},{\"id\": \"CopySVG\", \"label\": \"Copy SVG\"},{\"id\": \"ViewSVG\", \"label\": \"View SVG\"},{\"id\": \"ViewSource\", \"label\": \"View Source\"},{\"id\": \"SaveAs\", \"label\": \"Save As\"},null,{\"id\": \"Help\"},{\"id\": \"About\", \"label\": \"About Adobe CVG Viewer...\"}]}}"
        , 22)

-- example from json-schema.org/examples.html
test3 :: TestCase
test3 = ("{\"$schema\": \"http://json-schema.org/draft-04/schema#\",\"type\": \"object\",\"properties\": {\"/\": {}},\"patternProperties\": {\"^(/[^/]+)+$\": {}},\"additionalProperties\": false,\"required\": [ \"/\" ]}"
        , 6)

-- example from json-schema.org/example2.html
test4 :: TestCase
test4 = ("{\"id\": \"http://some.site.somewhere/entry-schema#\",\"$schema\": \"http://json-schema.org/draft-04/schema#\",\"description\": \"schema for an fstab entry\",\"type\": \"object\",\"required\": [ \"storage\" ],\"properties\": {\"storage\": {\"type\": \"object\",\"oneOf\": [{ \"$ref\": \"#/definitions/diskDevice\" },{ \"$ref\": \"#/definitions/diskUUID\" },{ \"$ref\": \"#/definitions/nfs\" },{ \"$ref\": \"#/definitions/tmpfs\" }]},\"fstype\": {\"enum\": [ \"ext3\", \"ext4\", \"btrfs\" ]},\"options\": {\"type\": \"array\",\"minItems\": 1,\"items\": { \"type\": \"string\" },\"uniqueItems\": true},\"readonly\": { \"type\": \"boolean\" }},\"definitions\": {\"diskDevice\": {},\"diskUUID\": {},\"nfs\": {},\"tmpfs\": {}}}"
        , 7)

-- example from sitepoint.com/facebook-json-example/
test5 :: TestCase
test5 = ("{\"data\": [{\"id\": \"X999_Y999\",\"from\": {\"name\": \"Tom Brady\", \"id\": \"X12\"},\"message\": \"Looking forward to 2010!\",\"actions\": [{\"name\": \"Comment\",\"link\": \"http://www.facebook.com/X999/posts/Y999\"},{\"name\": \"Like\",\"link\": \"http://www.facebook.com/X999/posts/Y999\"}],\"type\": \"status\",\"created_time\": \"2010-08-02T21:27:44+0000\",\"updated_time\": \"2010-08-02T21:27:44+0000\"},{\"id\": \"X998_Y998\",\"from\": {\"name\": \"Peyton Manning\", \"id\": \"X18\"},\"message\": \"Where's my contract?\",\"actions\": [{\"name\": \"Comment\",\"link\": \"http://www.facebook.com/X998/posts/Y998\"},{\"name\": \"Like\",\"link\": \"http://www.facebook.com/X998/posts/Y998\"}],\"type\": \"status\",\"created_time\": \"2010-08-02T21:27:44+0000\",\"updated_time\": \"2010-08-02T21:27:44+0000\"}]}"
        , 7)

testAll :: IO ()
testAll = mapM_ tester [test1, test2, test3, test4, test5]

testRandom :: IO ()
testRandom = do
    json <- generateObject
    print json
    print $ degreeJSON json

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