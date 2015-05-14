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
    (Absolute h, HostRelative) -> URL {
            url_type = url_type base,
            url_path = url_path url,
            url_params = url_params url
        }--createURL $ T.pack $ exportHost h ++ exportURL url
    (Absolute h, PathRelative) -> URL {
            url_type = url_type base,
            url_path = url_path url,
            url_params = url_params url
        }--createURL $ T.pack $ exportHost h ++ ('/' : exportURL url)
    otherwise -> url

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
    return $ foldl (\set link -> Set.insert (createURL link) set) Set.empty textLinks

referencesClosure :: URL -> IO [(URL, [URL])]
referencesClosure rootUrl = referencesClosure' [rootUrl] (Set.singleton rootUrl)
    where
        referencesClosure' [] _ = return []
        referencesClosure' (url : queue) set = do
            linkSet <- relatedLinks $ resolveURL rootUrl url
            let links = Set.elems linkSet
            print url
            putStrLn "Refs:"
            mapM_ print links
            putStrLn "___"
            let newLinks = filter (flip Set.notMember set) links
            let newQueue = queue ++ newLinks
            --let newSet = Set.union set $ Set.fromList newLinks
            let newSet = foldl (flip Set.insert) set newLinks
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
    let link = "http://progmeistars.lv/index.php?lang=ru"
    graph <- webGraph $ createURL link
    return ()
    --let webGraph = graphFromEdges graphList