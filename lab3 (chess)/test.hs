import qualified GenericTrie as Trie

main = do
	let keys = [["b"], ["a"], ["c"], ["a", "b", "c", "d"], ["d", "a"], ["b", "c"], ["b", "c"]]
	let vals = [1..(length keys)] :: [Int]
	let l = zip keys vals
	let tr = Trie.fromList l
	--print $ Trie.fold (\n s -> show n ++ (' ':s)) [] tr
	--print $ Trie.foldMaybe (\n s -> show n ++ (' ':s)) [] tr
	--print $ Trie.fold (\n s -> n ++ (' ':s)) [] $ Trie.map show tr
	--print $ Trie.fold (\n s -> n ++ (' ':s)) [] $ Trie.mapMaybe (\x -> Just $ show x) tr