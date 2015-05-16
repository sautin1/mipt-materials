{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Array ((!))
import Data.Graph
import qualified Data.Set as Set
import Data.List (isInfixOf, isSuffixOf, dropWhileEnd)
import Data.List.Ordered (sortOn)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Network.URL
import Network.HTTP.Conduit hiding (host)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, hasAttribute, laxAttribute, laxElement, fromDocument, ($//), (>=>), check)
import Network (withSocketsDo)
import Data.Maybe (fromJust, isNothing)
import Control.Monad (filterM)
import Control.Monad.Par (parMap, runPar) -- for enabling parMap
import Control.Exception (try)
import Control.Exception.Base (evaluate)

fst3 (x, _, _) = x
snd3 (_, x, _) = x
trd3 (_, _, x) = x

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

loadSource :: URL -> IO B.ByteString
loadSource url = withSocketsDo $ simpleHttp $ exportURL url

cursorFor :: URL -> IO Cursor
cursorFor url = do
		page <- loadSource url
		return $ fromDocument $ parseLBS page

createURL :: T.Text -> Maybe URL
createURL = importURL . T.unpack

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

isGoodUrl :: URL -> IO Bool
isGoodUrl url = do
	let isNotMail = not (isMailto url)
	let urlPath = url_path url
	let isHtml = and (runPar $ parMap (\ext -> not $ ext `isSuffixOf` urlPath) 
		[	".png", ".jpg", ".gif", ".jpeg", ".pdf", 
			".doc", ".rtf", ".xls", ".pptx", ".pps", 
			".zip", ".rar", ".mp3", ".djvu"]
		)
	source <- loadSource url
	eithExc <- try (evaluate $ B.null source) :: IO (Either HttpException Bool)
	let isAccess = isRight eithExc
	return $ isNotMail && isHtml && isAccess


relatedLinks :: URL -> IO (Set.Set URL)
relatedLinks url = do
	cursor <- cursorFor url
	let textLinks = cursor 	$// laxElement "a"
							>=> laxAttribute "href"
	let links = map fromJust $ filter (not.isNothing) $ map createURL textLinks
	goodLinks <- filterM (isGoodUrl.(resolveURL url)) links
	let goodHostLinks = filter (hasSameDomain url) goodLinks
	return $ foldl (\set link -> Set.insert (makeHostRelative url link) set) Set.empty goodHostLinks
	--return $ foldl (\set link -> Set.insert (makeHostRelative url $ createURL link) set) Set.empty textLinks

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

startWork :: URL -> IO ()
startWork url = do
	putStrLn "Building web graph..."
	references <- referencesClosure url
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

	mapM_ 	(\(pr, key, keyL) -> do
				putStrLn $ "URL: " ++ exportURL key
				putStrLn $ "PR : " ++ show pr ++ "\n"
			) $ pagesInfo
	putStrLn "... finished!"

main :: IO ()
main = do
	putStrLn "Enter link: "
	link <- getLine
	--let link = "http://www.alieparusa.ru/"
	--let link = "http://student.progmeistars.lv/2009_06Alexey%20'popoffka'%20Popov/"
	--let link = "http://www.drive2.ru/" -- works too long since the site is enormous. Looks spectacular.
	--let link = "http://lio.lv" -- fails because of "..\..\default.htm"
	--let link = "http://progmeistars.lv/index.php?lang=ru" -- fails because of "/../olimpics.html"
	let mbUrl = createURL $ T.pack link
	case mbUrl of
		Nothing -> putStrLn "Cannot parse URL"
		Just url -> do
			isGood <- isGoodUrl url
			if isGood 	then startWork url
						else putStrLn "Cannot access URL"
