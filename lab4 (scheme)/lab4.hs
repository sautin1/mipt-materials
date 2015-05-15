{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Graph
import qualified Data.Set as Set
import Data.List (isInfixOf, isSuffixOf, dropWhileEnd)
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
    }
    (Absolute h, PathRelative) -> URL {
            url_type = url_type base,
            url_path = (dropWhileEnd (/= '/') $ url_path base) ++ url_path url,
            url_params = url_params url
        }
    otherwise -> url

makeHostRelative :: URL -> URL -> URL
makeHostRelative base url = URL {
        url_type = HostRelative,
        url_path = url_path resolvedUrl,
        url_params = url_params resolvedUrl
    }
        where resolvedUrl = resolveURL base url

hasSameDomain :: URL -> URL -> Bool
hasSameDomain base url = case (url_type base, url_type url) of 
    (Absolute _, PathRelative) -> True
    (Absolute _, HostRelative) -> True
    (Absolute bh, Absolute uh) -> bh == uh
    otherwise -> False

isMailto :: URL -> Bool
isMailto = (isInfixOf "mailto:") . url_path

isGoodUrl :: URL -> URL -> Bool
isGoodUrl base url = hasSameDomain base url && not (isMailto url)
    && and (map (\ext -> not $ ext `isSuffixOf` urlPath) [  ".png", ".jpg", ".gif", ".jpeg", ".pdf", 
                                                            ".doc", ".rtf", ".xls", ".pptx", ".pps", 
                                                            ".zip", ".rar", ".mp3"])
        where urlPath = url_path url

relatedLinks :: URL -> IO (Set.Set URL)
relatedLinks url = do
    cursor <- cursorFor url
    let textLinks = cursor  $// laxElement "a"
                            >=> hasAttribute "href"
                            >=> check ((isGoodUrl url) . createURL . (T.concat) . attribute "href")
                            >=> attribute "href"
    return $ foldl (\set link -> Set.insert (makeHostRelative url $ createURL link) set) Set.empty textLinks

referencesClosure :: URL -> IO [(URL, [URL])]
referencesClosure rootUrl = referencesClosure' [relativeRootUrl] (Set.singleton $ relativeRootUrl)
    where
        relativeRootUrl = makeHostRelative rootUrl rootUrl

        referencesClosure' [] _ = return []
        referencesClosure' (url : queue) set = do
            putStrLn $ "\n\tParsing " ++ exportURL url
            linkSet <- relatedLinks $ resolveURL rootUrl url
            let links = Set.elems linkSet
            let addSet = Set.difference linkSet set
            let newSet = Set.union set addSet
            let newLinks = Set.elems addSet
            let newQueue = queue ++ newLinks
            putStr $ "\tQueue: "
            print $ length newQueue
            sonRes <- referencesClosure' newQueue newSet
            --print (url, links)
            --putStrLn ""
            return $ (url, links) : sonRes

pageRank :: (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex) -> (Graph, Int)
pageRank (gr, unhash, hash) = (gr, 0)
--  | 
--      where (newGr, maxDiff) = pageRankIter gr
--          where 
--              pageRankIter 
--                  where
--                      dampFact = 0.85
--                      eps = 0.5


main :: IO()
main = do
    putStrLn "Enter link: "
    --link <- getLine
    let link = "http://www.alieparusa.ru/"
    --let link = "http://student.progmeistars.lv/2009_06Alexey%20'popoffka'%20Popov/"
    --let link = "http://www.drive2.ru/" -- works too long since the site is enormous. Looks spectacular.
    --let link = "http://lio.lv" -- fails because of "..\..\default.htm"
    --let link = "http://progmeistars.lv/index.php?lang=ru" -- fails because of "/../olimpics.html"
    
    putStrLn "Building web graph..."
    references <- referencesClosure $ createURL $ T.pack link
    let forGraph = zipWith (\(l, ls) node -> (node, l, ls)) references [0,0..]
    let grRes@(webGraph, unhash, hash) = graphFromEdges forGraph
    putStr "... web graph of "
    putStr $ show $ length $ vertices webGraph
    putStr " nodes and "
    putStr $ show $ length $ edges webGraph
    putStrLn " edges is built!"

    putStrLn "Counting PageRank..."
    let prGraph = pageRank grRes
    putStrLn "... finished!"
    return ()
