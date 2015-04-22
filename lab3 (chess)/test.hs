import qualified ChessTrie as Trie

main = do
	--let keys = [["b"], ["a"], ["c"], ["a", "b", "c", "d"], ["d", "a"], ["b", "c"], ["b", "c"]]
	let keys = [["a"], ["a", "b", "c", "d"], ["b", "c", "d"], ["a", "b", "c"], ["b", "c", "a"]]
	let wins = [0, 1, 0, 1, 0, 2, -3]
	let games = zip keys wins
	--print $ Trie.insert (head games) Trie.empty
	let trie = Trie.fromList games
	putStrLn $ Trie.show trie
	putStrLn ""

	print $ Trie.longestOpen trie

	print $ Trie.lookup ["a", "b", "d"] trie
	--print $ Trie.foldMaybe (\n s -> show n ++ (' ':s)) [] tr
	--print $ Trie.fold (\n s -> n ++ (' ':s)) [] $ Trie.map show tr
	--print $ Trie.fold (\n s -> n ++ (' ':s)) [] $ Trie.mapMaybe (\x -> Just $ show x) tr