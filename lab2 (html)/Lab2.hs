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

grabName :: Cursor -> IO T.Text
grabName cursor = do
    return $ T.concat $
        cursor  $// element "h1"
                >=> attributeIs "class" "firstHeading"
                &// content

grabContact :: Cursor -> T.Text -> IO T.Text
grabContact cursor str = do
    return $ T.concat $
        cursor  $// element "table" 
                >=> attributeIs "class" "wikitable card"
                &// element "a"
                >=> check (\c -> T.isInfixOf str $ T.concat $ attribute "href" c)
                >=> attribute "href"

grabTeacher :: Url -> IO TeacherContact
grabTeacher url = do
    cursor <- cursorFor url
    name <- grabName cursor >>= (return . T.unpack)
    [vkUrl, fbUrl, linkedUrl] <- mapM (grabContact cursor) ["vk.com", "facebook.com", "linkedin.com"] >>= (return . (map T.unpack))
    return $ TeacherContact name vkUrl fbUrl linkedUrl

nextPage :: Url -> IO Url
nextPage url = do
    cursor <- cursorFor url
    let next = cursor   $// element "div"
                        >=> attributeIs "id" "mw-pages"
                        &/ element "a"
                        &/ check (\c -> content c == ["следующие 200"])
                        >=> parent
                        &| T.concat . attribute "href"
    if (null next)  then return ""
                    else return $ head next
        
teacherLinks :: Url -> IO [Url]
teacherLinks "" = return []
teacherLinks url = do
    cursor <- cursorFor url
    return $
        cursor  $// element "div" 
                >=> attributeIs "id" "mw-pages" 
                &// element "div"
                >=> attributeIs "class" "mw-content-ltr" 
                &// element "a" 
                &| T.concat . attribute "href"


-- == for testing == --
printGrabTeacher url = do
    contact <- grabTeacher url
    print contact

printNextPage url = do
    link <- nextPage url
    putStrLn $ T.unpack link

printTeacherLinks = do
    links <- teacherLinks url0
    mapM_ (putStrLn . (T.unpack)) links
-- == ___________ == --

lab2 :: IO [T.Text]
lab2 = do
    return []
    --teacherLinks url0 >>= grabTeacher
    

main :: IO()
main = withSocketsDo $ do
    nodes <- lab2
    dir <- getCurrentDirectory
    initReq <- parseUrl "http://mipt.eu01.aws.af.cm/lab2"
    handle <- openFile (dir ++ "/Lab2.hs") ReadMode
    hSetEncoding handle utf8_bom
    content <- hGetContents handle
    let req = urlEncodedBody [("email", email), ("result", encodeUtf8 $ T.concat $ nodes), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
    response <- withManager $ httpLbs req
    hClose handle
    L.putStrLn $ responseBody response
