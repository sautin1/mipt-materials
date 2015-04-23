import qualified ChessTrie as Trie

main = do
	let keys = [["b"], ["a"], ["c"], ["a", "b", "c", "d"], ["d", "a"], ["b", "c"], ["b", "c"]]
	--let keys = [["a"], ["a", "b", "c", "d"], ["b", "c", "d"], ["a", "b", "c"], ["b", "c", "a"]]
	let wins = [0, 1, 0, 1, 0, 2, -3]
	let games = zip keys wins
	putStrLn "Game:"
	print games
	putStrLn "Trie:"
	let trie = Trie.fromList games
	putStrLn $ Trie.show trie

	putStrLn "\nLongest opening:"
	--let longest = Trie.longestOpen trie
	let longest = Trie.traverse Trie.longestOpenVisit Trie.goDeeper ([], 0) ([], 0) trie
	print $ longest
	print $ Trie.lookup (fst longest) trie

	putStrLn "\nBest white opening:"
	let ww = Trie.traverse Trie.whiteOpenVisitWithLast Trie.goDeeper ([], 0.0) ([], 0) trie
	print $ ww
	print $ Trie.lookup (fst ww) trie

	putStrLn "\nBest black opening:"
	let ww1 = Trie.traverse Trie.blackOpenVisitWithLast Trie.goDeeper ([], 0.0) ([], 0) trie
	print $ ww1
	print $ Trie.lookup (fst ww1) trie

	putStrLn "\nBest opening for white:"
	let ww2 = Trie.traverse (Trie.winOpenVisit Trie.whiteProb) Trie.goDeeper ([], 0.0) ([], 0) trie
	print $ ww2
	print $ Trie.lookup (fst ww2) trie

	putStrLn "\nBest opening for black:"
	let ww3 = Trie.traverse (Trie.winOpenVisit Trie.blackProb) Trie.goDeeper ([], 0.0) ([], 0) trie
	print $ ww3
	print $ Trie.lookup (fst ww3) trie

