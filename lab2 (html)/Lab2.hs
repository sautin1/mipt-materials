{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Encoding
import Network.HTTP.Conduit
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attribute, attributeIs, content, element, fromDocument, child, ($/), ($//), (&|), (&//), (&/), (>=>), node, check, parent) 
import Network (withSocketsDo)

type Url = T.Text
type Contact = String

data TeacherContact = TeacherContact {
                        name    :: String,
                        vk      :: Contact,
                        fb      :: Contact,
                        lin     :: Contact
                    } deriving Eq

-- почтовый адрес
email = ""
urlPrefix = "http://wikimipt.org"
url0 = T.append urlPrefix "/index.php?title=%D0%9A%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D1%8F:%D0%9F%D1%80%D0%B5%D0%BF%D0%BE%D0%B4%D0%B0%D0%B2%D0%B0%D1%82%D0%B5%D0%BB%D0%B8_%D0%BF%D0%BE_%D0%B0%D0%BB%D1%84%D0%B0%D0%B2%D0%B8%D1%82%D1%83"

instance Show TeacherContact where
    show tc = "name: " ++ name tc
        ++ "\nvkontakte: " ++ vk tc
        ++ "\nfacebook: " ++ fb tc
        ++ "\nlinkedIn: " ++ lin tc

cursorFor :: Url -> IO Cursor
cursorFor url = do
        page <- withSocketsDo $ simpleHttp $ T.unpack $ url
        return $ fromDocument $ parseLBS page

grabName :: Cursor -> T.Text
grabName cursor = T.concat $ cursor $// element "h1"
                                    >=> attributeIs "class" "firstHeading"
                                    &// content

grabContact :: Cursor -> T.Text -> T.Text
grabContact cursor str = T.concat $ cursor  $// element "table"
                                            >=> attributeIs "class" "wikitable card"
                                            &// element "a"
                                            >=> check (\c -> T.isInfixOf str $ T.concat $ attribute "href" c)
                                            >=> attribute "href"

grabTeacher :: Cursor -> TeacherContact
grabTeacher cursor = TeacherContact name vkUrl fbUrl linkedUrl
    where
        name = T.unpack $ grabName cursor
        [vkUrl, fbUrl, linkedUrl] = map (T.unpack . grabContact cursor) ["vk.com", "facebook.com", "linkedin.com"]

nextPage :: Cursor -> Url
nextPage cursor
    | null next = ""
    | otherwise = T.replace "&amp;" "&" $ T.append urlPrefix $ head next
        where next = cursor $// element "div"
                            >=> attributeIs "id" "mw-pages"
                            &/ element "a"
                            &/ check (\c -> content c == ["следующие 200"])
                            >=> parent
                            &| T.concat . attribute "href"

teacherLinksPage :: Cursor -> [Url]
teacherLinksPage cursor = cursor    $// element "div" 
                                    >=> attributeIs "id" "mw-pages" 
                                    &// element "div"
                                    >=> attributeIs "class" "mw-content-ltr" 
                                    &// element "a" 
                                    &| T.concat . attribute "href"


teacherLinksAll :: Url -> IO [Url]
teacherLinksAll "" = return []
teacherLinksAll url = do
    cursor <- cursorFor url
    let thisPageTeachers = map (T.append urlPrefix) $ teacherLinksPage cursor
    nextPageTeachers <- teacherLinksAll $  nextPage cursor
    return $ thisPageTeachers ++ nextPageTeachers

lab2 :: IO [T.Text]
lab2 = do
    cursors <- teacherLinksAll url0 >>= mapM (cursorFor)
    return $ map (T.pack . show . grabTeacher) cursors

-- == for testing == --
printGrabTeacher url = do
    cursor <- cursorFor url
    print $ grabTeacher cursor

printNextPage url = do
    cursor <- cursorFor url
    putStrLn $ T.unpack $ nextPage cursor

printTeacherLinks = do
    links <- teacherLinksAll url0
    mapM_ (putStrLn . (T.unpack)) links

printAll = do
    res <- lab2
    putStrLn $ T.unpack $ T.intercalate "\n\n" res
-- == ___________ == -- 

main :: IO()
main = do
    putStrLn "start"
    printAll{-withSocketsDo $ do
    nodes <- lab2
    dir <- getCurrentDirectory
    initReq <- parseUrl "http://mipt.eu01.aws.af.cm/lab2"
    handle <- openFile (dir ++ "/Lab2.hs") ReadMode
    hSetEncoding handle utf8_bom
    content <- hGetContents handle
    let req = urlEncodedBody [("email", email), ("result", encodeUtf8 $ T.concat $ nodes), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
    response <- withManager $ httpLbs req
    hClose handle
    L.putStrLn $ responseBody response-}
