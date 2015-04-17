--(email, name) = ("sautin@phystech.edu", encodeUtf8 "Саутин А.А. (2)")

import System.IO
import System.Environment
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Trie as Trie
import qualified Data.ByteString as B

type Turn = String
type Game = ([Turn], Bool) -- (turns, whiteWins)
type Opening = B.ByteString -- concatenation of turns-strings intercalated by space
type OpeningData = (Opening, Bool) -- opening + whiteWins
type TrieNodeInfo = (Integer, Bool)

buildGame :: String -> Game
buildGame s = (turnsStr, gameResult)
    where
        (rawTurns, rawResult) = break (\x -> head x == '{') $ words s
        turnsStr = filter (\x -> Char.isLetter $ head x) rawTurns
        gameResult = head (last rawResult) == '1'

createTrieNodeInfo :: [OpeningData] -> [(Opening, TrieNodeInfo)]
createTrieNodeInfo = map (\x -> (fst x, (1, snd x)))

main = do
    args <- getArgs
    handle <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle
    let usefulData = filter (\s -> (s /= "\r") && (head s /= '[')) $ lines contents
    let gameSet = map buildGame usefulData
    let openings = map (List.intercalate " " . fst) gameSet
    let openingDataList = zip openings $ map snd gameSet
    let trie = Trie.fromList $ createTrieNodeInfo openingDataList
    --print Trie.member 
    --let counter = Map.filter (> 1) $ Map.unionsWith (+) inCounter
    --print $ Map.size counter
    hClose handle
