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
	traverse,
	goDeeper,
	reverseResult,
	whiteProb,
	blackProb,
	longestOpenVisit,
	winOpenVisit,
	whiteOpenVisitWithLast,
	blackOpenVisitWithLast
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

-- basic trie functions

empty :: ChessTrie
empty = ChessTrie (Map.empty) $ TrieValue 0 0 0 ""

lookup :: Opening -> ChessTrie -> Maybe TrieValue
lookup [] tr = Just (value tr)
lookup (t:ts) tr = 
	case subtrie of 
		Just subt -> ChessTrie.lookup ts subt
		Nothing -> Nothing
		where
			subtrie = Map.lookup t $ trans tr

-- leave it private (do not export)
add v1 newKey v2 = TrieValue (occW v1 + (occW v2)) (occB v1 + (occB v2)) (occTot v1 + (occTot v2)) newKey

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

-- traverses trie, accumulating two values: acc - value we want to get after the traverse, cur - state of the node (e.g. (key, depth))
traverse :: (a -> b -> a -> ChessTrie -> a) -> (ChessTrie -> b -> b) -> a -> b -> ChessTrie -> a
traverse visit newCur stA stC trie = traverse' stA stC trie
	where
		traverse' acc cur tr = visit acc cur sub tr
			where
				sub = fst $ Map.fold f (acc, cur) $ trans tr
					where
						f tr (acc', cur') = (traverse' acc' (newCur tr cur') tr, cur')

-- auxiliary util for winOpenVisit and longestOpenVisit
goDeeper :: ChessTrie -> (Opening, Integer) -> (Opening, Integer)
goDeeper tr (kl, d) = ((key $ value tr) : kl, d + 1) -- opening needs to be reverted
--goDeeper tr (kl, d) = (kl ++ [key $ value tr], d + 1) -- works slower

-- should be used after goDeeper to get correct opening
reverseResult :: (Opening, Integer) -> (Opening, Integer)
reverseResult (op, n) = (reverse op, n)

-- functions for winOpenVisit

whiteProb :: ChessTrie -> Float
whiteProb tr = (fromInteger $ occW $ value tr) / (fromInteger $ occTot $ value tr)

blackProb :: ChessTrie -> Float
blackProb tr = (fromInteger $ occB $ value tr) / (fromInteger $ occTot $ value tr)

-- visitors for traverse

longestOpenVisit :: (Opening, Integer) -> (Opening, Integer) -> (Opening, Integer) -> ChessTrie -> (Opening, Integer)
longestOpenVisit acc@(accK, accD) cur@(curK, curD) sub@(subK, subD) tr
	| (occTot $ value tr) <= 1 = acc 								-- not opening
	| (accD <= subD) && (curD <= subD) = sub			 			-- opening, subP - good depth
	| (subD <= curD) && (accD <= curD) = cur 						-- opening, curP - good depth
	| otherwise = acc 												-- opening, accP - good depth

-- finds best opening for a player
winOpenVisit :: (ChessTrie -> Float) -> (Opening, Float) -> (Opening, Integer) -> (Opening, Float) -> ChessTrie -> (Opening, Float)
winOpenVisit prob acc@(accK, accP) cur@(curK, curD) sub@(subK, subP) tr
	| (occTot $ value tr) <= 1 = acc 								-- not opening
	| (Map.null $ trans tr) && (accP <= curP) = (curK, curP)		-- opening, leaf, 		curP - good probability
	|  Map.null $ trans tr = acc 		 							-- opening, leaf, 		curP - bad probability
	|  curD == 0 = sub 			 									-- opening, not leaf,	root
	| (accP <= subP) && (curP <= subP) = sub			 			-- opening, not leaf, 	not root, subP - good probability
	| (subP <= curP) && (accP <= curP) = (curK, curP)				-- opening, not leaf, 	not root, curP - good probability
	| otherwise = acc 												-- opening, not leaf, 	not root, accP - good probability
		where
			curP = prob tr

-- finds best opening for 'white' where a turn of 'white' is the last one
whiteOpenVisitWithLast :: (Opening, Float) -> (Opening, Integer) -> (Opening, Float) -> ChessTrie -> (Opening, Float)
whiteOpenVisitWithLast acc@(accK, accP) cur@(curK, curD) sub@(subK, subP) tr
	| (occTot $ value tr) <= 1 = acc 								-- not opening
	| (Map.null $ trans tr) && (curD `mod` 2 /= 1) = acc 			-- opening, leaf, 		not white's turn
	| (Map.null $ trans tr) && (accP <= curP) = (curK, curP)		-- opening, leaf, 		white's turn, 		curP - good probability
	|  Map.null $ trans tr = acc 		 							-- opening, leaf, 		white's turn, 		curP - bad probability
	| (curD `mod` 2 /= 1) && (accP <= subP) = sub					-- opening, not leaf, 	not white's turn,	accP - bad probability
	|  curD `mod` 2 /= 1 = acc 										-- opening, not leaf, 	not white's turn, 	accP - good probability
	| (accP <= subP) && (curP <= subP) = sub			 			-- opening, not leaf, 	white's turn,		not root, subP - good probability
	| (subP <= curP) && (accP <= curP) = (curK, curP)				-- opening, not leaf, 	white's turn,		not root, curP - good probability
	| otherwise = acc 												-- opening, not leaf, 	white's turn,		not root, accP - good probability
		where
			curP = (fromInteger $ occW $ value tr) / (fromInteger $ occTot $ value tr)

-- finds best opening for 'black' where a turn of 'black' is the last one
blackOpenVisitWithLast :: (Opening, Float) -> (Opening, Integer) -> (Opening, Float) -> ChessTrie -> (Opening, Float)
blackOpenVisitWithLast acc@(accK, accP) cur@(curK, curD) sub@(subK, subP) tr
	| (occTot $ value tr) <= 1 = acc 								-- not opening
	| (Map.null $ trans tr) && (curD `mod` 2 /= 0) = acc 			-- opening, leaf, 		not black's turn
	| (Map.null $ trans tr) && (accP <= curP) = (curK, curP)		-- opening, leaf, 		black's turn, 		curP - good probability
	|  Map.null $ trans tr = acc 		 							-- opening, leaf, 		black's turn, 		curP - bad probability
	| (curD `mod` 2 /= 0) && (accP <= subP) = sub					-- opening, not leaf, 	not black's turn,	accP - bad probability
	|  curD `mod` 2 /= 0 = acc 										-- opening, not leaf, 	not black's turn, 	accP - good probability
	|  curD == 0 = sub 			 									-- opening, not leaf, 	black's turn, 		root
	| (accP <= subP) && (curP <= subP) = sub			 			-- opening, not leaf, 	black's turn,		subP - good probability
	| (subP <= curP) && (accP <= curP) = (curK, curP)				-- opening, not leaf, 	black's turn,		curP - good probability
	| otherwise = acc 												-- opening, not leaf, 	black's turn,		accP - good probability
		where
			curP = (fromInteger $ occB $ value tr) / (fromInteger $ occTot $ value tr)