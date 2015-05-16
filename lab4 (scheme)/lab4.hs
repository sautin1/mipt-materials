{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Array ((!))
import Data.Graph
import qualified Data.Set as Set
import Data.List (isInfixOf, isSuffixOf, dropWhileEnd)
import Data.List.Ordered (sortOn)
import qualified Data.Text as T
import Network.URL
import Network.HTTP.Conduit hiding (host)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, hasAttribute, attribute, laxElement, fromDocument, ($//), (>=>), check)
import Network (withSocketsDo)
import Data.Maybe (fromJust)

import Control.Monad.Par (parMap, runPar) -- for enabling parMap

fst3 (x, _, _) = x
snd3 (_, x, _) = x
trd3 (_, _, x) = x

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
    && and (runPar $ parMap (\ext -> not $ ext `isSuffixOf` urlPath) [  ".png", ".jpg", ".gif", ".jpeg", ".pdf", 
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
            return $ (url, links) : sonRes

pageRank :: (Graph, Vertex -> (Double, key, [key])) -> Vertex -> (Vertex -> (Double, key, [key]), Double)
pageRank (graph, unhash) vertex = (newUnh, newPr - pr)
    where
        (pr, k, ks) = unhash vertex
        newUnh v
            | v == vertex = (newPr, k, ks)
            | otherwise = unhash v
        newPr = foldl accumulatePr 0 $ trGr ! vertex
            where
                trGr = transposeG graph
                accumulatePr acc v = (1 - dumpFact) + dumpFact * (fst3 $ unhash v) / (outDeg v) + acc
                outDeg v = fromIntegral $ (outdegree graph) ! v
                dumpFact = 0.85

pageRanks :: (Graph, Vertex -> (Double, key, [key])) -> IO (Vertex -> (Double, key, [key]), Int)
pageRanks (graph, unhash) = pageRanks' unhash 1
    where
        pageRanks' unh n
            | maxDiff < eps = return (newUnh, n)
            | otherwise = do
                    putStr "\tIteration "
                    print n
                    putStr "\t\twith maxDiff = "
                    print maxDiff
                    pageRanks' newUnh $ n + 1
            where
                eps = 0.01
                (newUnh, maxDiff) = pageRanksIter unh
                    where
                        pageRanksIter un = foldl updatePrRes (un, 0.0) $ vertices graph
                            where
                                updatePrRes (accU, accD) v = (curU, max accD $ abs curD)
                                    where
                                        (curU, curD) = pageRank (graph, accU) v

main :: IO()
main = do
    putStrLn "Enter link: "
    link <- getLine
    --let link = "http://www.alieparusa.ru/"
    --let link = "http://student.progmeistars.lv/2009_06Alexey%20'popoffka'%20Popov/"
    --let link = "http://www.drive2.ru/" -- works too long since the site is enormous. Looks spectacular.
    --let link = "http://lio.lv" -- fails because of "..\..\default.htm"
    --let link = "http://progmeistars.lv/index.php?lang=ru" -- fails because of "/../olimpics.html"
    
    putStrLn "Building web graph..."
    references <- referencesClosure $ createURL $ T.pack link
    let forGraph = zipWith (\(l, ls) node -> (node, l, ls)) references [0.0, 0.0..]
    let (webGraph, unhash) = graphFromEdges' forGraph
    putStr "... web graph of "
    putStr $ show $ length $ vertices webGraph
    putStr " nodes and "
    putStr $ show $ length $ edges webGraph
    putStrLn " edges is built!\n_____"

    putStrLn "Counting PageRank..."
    (prUnhash, iter) <- pageRanks (webGraph, unhash)
    putStrLn $ "... finished with " ++ show iter ++ " iterations!\n_____"

    putStrLn "Printing results..."
    let pagesInfo = sortOn (negate.fst3) $ map prUnhash $ vertices webGraph

    mapM_   (\(pr, key, keyL) -> do
                putStrLn $ "URL: " ++ exportURL key
                putStrLn $ "PR : " ++ show pr ++ "\n"
            ) $ pagesInfo
    putStrLn "... finished!"

    return ()
