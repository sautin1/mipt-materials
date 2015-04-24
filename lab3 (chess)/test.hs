import qualified ChessTrie as Trie

main = do
	let keys = [["b"], ["a"], ["c"], ["a", "b", "c", "d"], ["d", "a"], ["b", "c"], ["b", "c"]]
	--let keys = [["a"], ["a", "b", "c", "d"], ["b", "c", "d"], ["a", "b", "c"], ["b", "c", "a"]]
	let wins = [0, 1, 0, 1, 0, 2, -3]
	--let keys = [["a", "a"], ["a", "b"], ["a", "c"], ["b", "a"], ["b", "b"], ["b", "c"], ["b", "a"]]
	--let wins = [0, 10, -10, 1, 4, 3, 2]
	let games = zip keys wins
	putStrLn "Game:"
	print games
	putStrLn "Trie:"
	let trie = Trie.fromList games
	putStrLn $ Trie.show trie

	putStrLn "\nLongest opening:"
	let longest = Trie.reverseResult $ Trie.traverse Trie.longestOpenVisit Trie.goDeeper ([], 0) ([], 0) trie
	print $ longest
	print $ Trie.lookup (fst longest) trie

	putStrLn "\nBest white opening:"
	let bw = Trie.reverseResult $ Trie.traverse (Trie.winOpenWithLastVisit Trie.whiteTurn Trie.whiteProb) Trie.goDeeper 
		([], 0.0) ([], 0) trie
	print $ bw
	print $ Trie.lookup (fst bw) trie

	putStrLn "\nBest black opening:"
	let bb = Trie.reverseResult $ Trie.traverse (Trie.winOpenWithLastVisit Trie.blackTurn Trie.blackProb) Trie.goDeeper 
		([], 0.0) ([], 0) trie
	print $ bb
	print $ Trie.lookup (fst bb) trie

	putStrLn "\nBest opening for white:"
	let bfw = Trie.reverseResult $ Trie.traverse (Trie.winOpenVisit Trie.whiteProb) Trie.goDeeper ([], 0.0) ([], 0) trie
	print $ bfw
	print $ Trie.lookup (fst bfw) trie

	putStrLn "\nBest opening for black:"
	let bfb = Trie.reverseResult $ Trie.traverse (Trie.winOpenVisit Trie.blackProb) Trie.goDeeper ([], 0.0) ([], 0) trie
	print $ bfb
	print $ Trie.lookup (fst bfb) trie


	putStrLn "\nWorst white turn:"
	let ww = Trie.reverseResult $ Trie.traverse (Trie.worstTurnVisit Trie.whiteTurn Trie.whiteProb) (Trie.goDeeperProb Trie.whiteProb) 
		([], -1.0) (([], 0), 0.0) trie
	print $ ww
	print $ Trie.lookup (fst ww) trie

	putStrLn "\nWorst black turn:"
	let wb = Trie.reverseResult $ Trie.traverse (Trie.worstTurnVisit Trie.blackTurn Trie.blackProb) (Trie.goDeeperProb Trie.blackProb) 
		([], -1.0) (([], 0), 0.0) trie
	print $ wb
	print $ Trie.lookup (fst wb) trie
