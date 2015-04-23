module ChessTrie (
	Turn(..),
	Game(..),
	buildGame,
	TrieValue(..),
	ChessTrie(..),
	empty,
	ChessTrie.lookup,
	insert,
	fromList,
	ChessTrie.show,
	longestOpen,
	winOpening
) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Data.Maybe (isNothing)

type Turn = String
type Opening = [Turn]
type Game = (Opening, Int) -- (turns, whoWins), whoWins = 0 - white, whoWins = 1 - black, otherwise - draw

-- takes an ordinary string from chess game log and converts it to Game
-- string has format: "{number of the turn}. {turnW} {turnB} \{comment\} {game result}"
buildGame :: String -> Game
buildGame s = (turnsStr, gameResult)
	where
		(rawTurns, rawResult) = break (\x -> head x == '{') $ words s
		turnsStr = filter (\x -> Char.isLetter $ head x) rawTurns
		gameResult = read ls :: Int
			where
				ls = last (last rawResult) : []


data TrieValue = TrieValue {
					 occW	:: Integer,
					 occB	:: Integer,
					 occTot	:: Integer,
					 key	:: String
				 } deriving (Show, Eq)

data ChessTrie = ChessTrie {
					trans 	:: Map.Map Turn ChessTrie,
					value 	:: TrieValue
				} deriving (Show, Eq)

empty :: ChessTrie
empty = ChessTrie (Map.empty) $ TrieValue 0 0 0 ""

-- leave it private (do not export)
add v1 newKey v2 = TrieValue (occB v1 + (occB v2)) (occW v1 + (occW v2)) (occTot v1 + (occTot v2)) newKey

lookup :: Opening -> ChessTrie -> Maybe TrieValue
lookup [] tr = Just (value tr)
lookup (t:ts) tr = 
	case subtrie of 
		Just subt -> ChessTrie.lookup ts subt
		Nothing -> Nothing
		where
			subtrie = Map.lookup t $ trans tr

insert :: Game -> ChessTrie -> ChessTrie
insert (tList, ww) = insert' tList (initVal ww) ""
	where
		initVal 0 = TrieValue 1 0 1 ""
		initVal 1 = TrieValue 0 1 1 ""
		initVal _ = TrieValue 0 0 1 ""

		insert' [] v prKey (ChessTrie tr oldV) = ChessTrie tr $ add v prKey oldV
		insert' (key:ks) v prKey (ChessTrie tr oldV) = ChessTrie (Map.alter alterTrie key tr) $ add v prKey oldV
			where
				alterTrie Nothing = Just (insert' ks v key empty)
				alterTrie (Just subTrie) = Just (insert' ks v key subTrie)

fromList :: [Game] -> ChessTrie
fromList = foldl (flip insert) ChessTrie.empty

show :: ChessTrie -> String
show tr = show' (Prelude.show . value) "" tr
	where 
		show' f st tr = st ++ (f tr) ++ "\n" ++ foldedSubtrie
			where
				foldedSubtrie = foldl (\acc t -> show' (("\t"++).f) acc t) "" . Map.elems $ trans tr

longestOpen :: ChessTrie -> (Opening, Int)
longestOpen = longest' ([], 0) ([], 0)
	where
		longest' acc@(bestK, bestD) cur@(curK, curD) tr
			| (occTot $ value tr) <= 1 = acc 					-- is not an opening, no openings lower -> fail
			| (Map.null $ trans tr) && (curD <= bestD) = acc 	-- is an opening, is a leaf, has bad depth
			| (Map.null $ trans tr) = cur 						-- is an opening, is a leaf, has good depth
			| subD <= curD = cur 								-- is an opening, not leaf, subtree has no better opening
			| otherwise = sub 									-- is an opening, not leaf, subtree has better opening
				where
					sub@(subK, subD) = fst $ Map.fold folderF (acc, cur) $ trans tr
						where
							folderF tr (a@(aK, aD), c@(cK, cD)) = (longest' a newCur tr, c)
								where
									newCur = (cK ++ [key $ value tr], cD + 1)


winOpening :: ChessTrie -> (Opening, Float)
winOpening = win' ([], 0.0) ([], 0)
	where
		win' acc@(bestK, bestP) cur@(curK, curD) tr
			| (occTot $ value tr) <= 1 						= acc	-- is not an opening, no openings lower -> fail
			| (Map.null $ trans tr) && (curP <= bestP) 		= acc	-- is an opening, is a leaf, has bad probability
			| (Map.null $ trans tr) && (curD == 0) 			= acc	-- is an opening, is a leaf, has good probability, no last turn (game has just started)
			| (Map.null $ trans tr) && (curD `mod` 2 == 1)  = acc	-- is an opening, is a leaf, has good probability, last is black's turn
			| (Map.null $ trans tr) 				= (curK, curP)	-- is an opening, is a leaf, has good depth
			| subP <= curP 							= (curK, curP)	-- is an opening, not leaf, subtree has no better opening
			| otherwise 							= sub			-- is an opening, not leaf, subtree has better opening
				where
					curP = (fromInteger $ occW $ value tr) / (fromInteger $ occTot $ value tr)
					sub@(subK, subP) = fst $ Map.fold folderF (acc, cur) $ trans tr
						where
							folderF tr (a, c@(cK, cD)) = (win' a newCur tr, c)
								where
									newCur = (cK ++ [key $ value tr], cD + 1)

--reverseOpening

----traverse :: ChessTrie
--traverse = whiteWinTrav ([], 0.0) ([], 0) sub
--	where
--		sub@(subK, subP) = fst $ Map.fold f (acc, cur) $ trans tr
--			where
--				f tr (a)
		

updateKeyDepth :: String -> (Opening, Integer) -> (Opening, Integer)
updateKeyDepth s (kl, d) = ((s:kl), d + 1)

whiteWinTrav :: (Opening, Float) -> (Opening, Integer) -> (Opening, Float) -> ChessTrie -> (Opening, Float)
whiteWinTrav acc@(accK, accP) cur@(curK, curD) sub@(subK, subP) tr
	| (occTot $ value tr) <= 1 = acc -- not opening
	| (Map.null $ trans tr) && (curD `mod` 2 /= 0) = acc -- opening, leaf, not white's turn
	| (Map.null $ trans tr) && (curP <= accP) = acc -- is an opening, leaf, white's turn, bad probability
	| (Map.null $ trans tr) = (curK, curP) -- opening, leaf, white's turn, good probability
	| curD == 0 = sub -- opening, not leaf, empty key
	| subP <= curP = (curK, curP) -- opening, not leaf, nonempty key, good probability
	| otherwise = sub -- opening, not leaf, nonempty key, bad probability
		where
			curP = (fromInteger $ occW $ value tr) / (fromInteger $ occTot $ value tr)