import System.Environment
import qualified Data.Set as Set
import qualified Data.List as List

-- launch with: ./dictMerger "../wordsWeb.txt" "../wordsNoNames.txt" "../dict.txt"
-- would be nice to remove \r from both files:
-- tr -d '\r' < inputfile > outputfile

main = do
	args <- getArgs
	contents1 <- readFile (args !! 0)
	contents2 <- readFile (args !! 1)
	let set1 = Set.fromList $ lines contents1
	let set2 = Set.fromList $ lines contents2
	let result = Set.union set1 set2
	print $ (Set.size result) - (Set.size set1)
	writeFile (args !! 2) (List.intercalate "\n" $ Set.toList result)