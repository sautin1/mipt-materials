{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Graph
import qualified Data.Set as Set
import qualified Data.Text as T
import Network.URL
import Network.HTTP.Conduit hiding (host)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, hasAttribute, attribute, laxElement, fromDocument, ($//), (>=>), check)
import Network (withSocketsDo)
import Data.Maybe (fromJust)

cursorFor :: URL -> IO Cursor
cursorFor url = do
        page <- withSocketsDo $ simpleHttp $ exportURL url
        return $ fromDocument $ parseLBS page

createURL :: T.Text -> URL
createURL = fromJust . importURL . T.unpack

resolveURL :: URL -> URL -> URL
resolveURL base url = case (url_type base, url_type url) of
    (_, Absolute _) -> url
    (Absolute h, _) -> URL {
            url_type = url_type base,
            url_path = url_path url,
            url_params = url_params url
        }
    otherwise -> url

makeRelative :: URL -> URL
makeRelative url = case url_type url of
    HostRelative -> url
    otherwise -> URL {
            url_type = HostRelative,
            url_path = url_path url,
            url_params = url_params url
        }

hasSameDomain :: URL -> URL -> Bool
hasSameDomain base url = case (url_type base, url_type url) of 
    (Absolute _, PathRelative) -> True
    (Absolute _, HostRelative) -> True
    (Absolute bh, Absolute uh) -> bh == uh
    otherwise -> False

relatedLinks :: URL -> IO (Set.Set URL)
relatedLinks url = do
    cursor <- cursorFor url
    let textLinks = cursor  $// laxElement "a"
                            >=> hasAttribute "href"
                            >=> check ((hasSameDomain url) . createURL . (T.concat) . attribute "href")
                            >=> attribute "href"
    return $ foldl (\set link -> Set.insert (makeRelative $ createURL link) set) Set.empty textLinks

referencesClosure :: URL -> IO [(URL, [URL])]
referencesClosure rootUrl = referencesClosure' [makeRelative rootUrl] (Set.singleton $ makeRelative rootUrl)
    where
        referencesClosure' [] _ = return []
        referencesClosure' (url : queue) set = do
            linkSet <- relatedLinks $ resolveURL rootUrl url
            let links = Set.elems linkSet
            putStrLn "\n\tTEST"
            print url
            putStrLn "\tRefs:"
            mapM_ print links
            putStrLn "__"
            let addSet = Set.difference linkSet set
            let newSet = Set.union set addSet
            let newLinks = Set.elems addSet
            putStrLn "\tnew:"
            mapM_ print newLinks
            let newQueue = queue ++ newLinks
            sonRes <- referencesClosure' newQueue newSet
            return $ (url, links) : sonRes

webGraph :: URL -> IO ()--IO Graph
webGraph url = do
    references <- referencesClosure url
    mapM_ (print . fst) references
    return ()

main :: IO()
main = do
    --link <- getLine
    --let link = "http://progmeistars.lv/index.php?lang=ru" -- some kind of error from server
    let link = "http://www.alieparusa.ru/"
    graph <- webGraph $ createURL link
    return ()
    --let webGraph = graphFromEdges graphList