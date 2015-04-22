--(email, name) = ("sautin@phystech.edu", encodeUtf8 "Саутин А.А. (2)")

import System.IO
import System.Environment
import qualified ChessTrie as Trie

main = do
	args <- getArgs
	handle <- openFile (args !! 0) ReadMode
	contents <- hGetContents handle
	let usefulData = filter (\s -> (s /= "\r") && (head s /= '[')) $ lines contents -- list of useful strings
	let gameList = map Trie.buildGame usefulData -- list of games
	let trie = Trie.fromList gameList

	putStrLn "-- finding longest opening --"
	let longest@(ls, ld) = Trie.longestOpen trie
	putStrLn "in format (opening, length):"
	print $ longest
	putStrLn "\nValue, stored in trie with this opening"
	putStrLn "(occW - white wins, occB - black wins,\noccTot - total games, key - last turn):"
	print $ Trie.lookup ls trie
	hClose handle
