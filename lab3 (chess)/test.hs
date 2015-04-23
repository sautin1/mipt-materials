import qualified ChessTrie as Trie

main = do
	--let keys = [["b"], ["a"], ["c"], ["a", "b", "c", "d"], ["d", "a"], ["b", "c"], ["b", "c"]]
	let keys = [["a"], ["a", "b", "c", "d"], ["b", "c", "d"], ["a", "b", "c"], ["b", "c", "a"]]
	let wins = [0, 1, 0, 1, 0, 2, -3]
	let games = zip keys wins
	let trie = Trie.fromList games
	putStrLn $ Trie.show trie
	putStrLn ""

	print $ Trie.longestOpen trie

	print $ Trie.lookup ["a", "b", "d"] trie

	print $ Trie.winOpening trie
