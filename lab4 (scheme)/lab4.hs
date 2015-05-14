{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Graph
import qualified Data.Set as Set
import qualified Data.Text as T
import Network.HTTP.Conduit hiding (host)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, hasAttribute, attribute, element, laxElement, fromDocument, ($//), (>=>), check)
import Network (withSocketsDo)
import Network.URL
import Data.Maybe (fromJust)

cursorFor :: URL -> IO Cursor
cursorFor url = do
        page <- withSocketsDo $ simpleHttp $ exportURL url
        return $ fromDocument $ parseLBS page

-- can throw errors
createURL :: T.Text -> URL
createURL = fromJust . importURL . T.unpack

resolveURL :: URL -> URL -> URL
resolveURL base rel = case url_type rel of
    Absolute _ -> rel
    otherwise -> case url_type base of
        Absolute dom -> createURL $ T.pack $ domain dom ++ normalize (exportURL rel)
            where
                domain dom = "http://" ++ host dom
                normalize [] = []
                normalize ur = if (head ur == '/') 
                    then ur
                    else '/' : ur
        otherwise -> rel

hasSameDomain :: URL -> URL -> Bool
hasSameDomain baseUrl url = case baseType of 
    Absolute baseHost -> case urlType of 
        Absolute urlHost -> baseHost == urlHost
        PathRelative -> True
        otherwise -> False
    PathRelative -> False
    otherwise -> False
    where
        baseType = url_type baseUrl
        urlType  = url_type url

relatedLinks :: URL -> IO (Set.Set URL)
relatedLinks url = do
    cursor <- cursorFor url
    let textLinks = cursor  $// laxElement "a"
                            >=> hasAttribute "href"
                            >=> check ((hasSameDomain url) . createURL . (T.concat) . attribute "href")
                            >=> attribute "href"
    return $ foldl (\set link -> Set.insert (createURL link) set) Set.empty textLinks

referencesClosure :: URL -> IO [(URL, [URL])]
referencesClosure url = referencesClosure' [url] (Set.singleton url)
    where
        referencesClosure' [] _ = return []
        referencesClosure' (u:q) set = do
            linkSet <- relatedLinks $ resolveURL url u
            let links = Set.elems linkSet
            print u
            putStrLn "Refs:"
            mapM_ print links
            putStrLn "___"
            let newLinks = filter (\x -> Set.member x set) links
            let newQueue = q ++ links
            let newSet = foldl (flip Set.insert) set newLinks
            son_fun <- referencesClosure' newQueue newSet
            return $ (u, links) : son_fun

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