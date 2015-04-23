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

	putStrLn ">> finding longest opening in format (opening, length)"
	let longest@(ls, ld) = Trie.reverseResult $ Trie.traverse Trie.longestOpenVisit Trie.goDeeper ([], 0) ([], 0) trie
	print $ longest
	putStrLn "\n>> Value, stored in trie with this opening"
	putStrLn ">> (occW - white wins, occB - black wins, occTot - total games, key - last turn)"
	print $ Trie.lookup ls trie
	hClose handle
