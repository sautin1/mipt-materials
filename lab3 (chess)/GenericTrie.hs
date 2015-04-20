module GenericTrie (
	Trie(..),
	empty,
	GenericTrie.null,
	leaf,
	implicit,
	insert,
	subtrie,
	fromList,
	foldMaybe,
	fold,
	mapMaybe,
	GenericTrie.map
) where

import qualified Data.Map as Map
import Data.Maybe (isNothing)

data Trie k v = Trie {
					trans :: Map.Map k (Trie k v),
					val :: Maybe v
				} deriving (Show, Eq)

empty :: Ord k => Trie k v
empty = Trie (Map.empty) Nothing

null :: Trie k v -> Bool
null t = (Map.null $ trans t) && (isNothing $ val t)

leaf :: Trie k v -> Bool
leaf = (Map.null).trans

implicit :: Trie k v -> Bool
implicit = (isNothing).val

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert [] val (Trie trans _) = Trie trans (Just val)
insert (key:ks) vNew trie = Trie (Map.alter alterTrie key $ trans trie) $ val trie
	where 
		alterTrie Nothing = Just (insert ks vNew empty)
		alterTrie (Just subTrie) = Just (insert ks vNew subTrie)

subtrie :: Ord k => [k] -> Trie k v -> Maybe (Trie k v)
subtrie [] tr = Just tr
subtrie (key:ks) tr = 
	case mbSubtr of
		Nothing -> Nothing
		(Just subtr) -> subtrie ks subtr
	where mbSubtr = Map.lookup key $ trans tr

fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldl (\acc (ky, vl) -> insert ky vl acc) empty

foldMaybe :: (Maybe v -> a -> a) -> a -> Trie k v -> a
foldMaybe f st tr = f (val tr) foldedSubtrie
	where foldedSubtrie = Map.fold (\t acc -> foldMaybe f acc t) st $ trans tr

fold :: (v -> a -> a) -> a -> Trie k v -> a
fold f = foldMaybe f'
	where
		f' Nothing acc = acc
		f' (Just vl) acc = f vl acc

mapMaybe :: (Maybe v -> Maybe a) -> Trie k v -> Trie k a
mapMaybe f tr = Trie mappedSubtrie (f $ val tr)
	where mappedSubtrie = Map.map (mapMaybe f) $ trans tr

map :: (v -> a) -> Trie k v -> Trie k a
map f tr = mapMaybe f' tr
	where
		f' Nothing = Nothing
		f' (Just vl) = Just (f vl)

-- also works, but is not connected with foldMaybe
--fold :: (v -> a -> a) -> a -> Trie k v -> a
--fold f st tr = 
--	case (val tr) of
--		Nothing -> foldedSubtrie
--		Just vl -> f vl foldedSubtrie
--	where foldedSubtrie = Map.fold (\t acc -> fold f acc t) st $ trans tr