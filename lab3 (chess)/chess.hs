--(email, name) = ("sautin@phystech.edu", encodeUtf8 "Саутин А.А. (2)")

import System.IO
import System.Environment
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Trie as Trie
import qualified Data.ByteString.Char8 as B

type Turn = String
type Game = ([Turn], Bool) -- (turns, whiteWins)
type Opening = B.ByteString -- concatenation of turns-strings intercalated by space
type TrieNodeInfo = (Integer, Bool)

buildGame :: String -> Game
buildGame s = (turnsStr, gameResult)
    where
        (rawTurns, rawResult) = break (\x -> head x == '{') $ words s
        turnsStr = filter (\x -> Char.isLetter $ head x) rawTurns
        gameResult = head (last rawResult) == '1'

createTrieNodeInfo :: (String, Bool) -> (Opening, TrieNodeInfo)
createTrieNodeInfo (s, ww) = (B.pack $ s, (1, ww))

main = do
    args <- getArgs
    handle <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle
    let usefulData = filter (\s -> (s /= "\r") && (head s /= '[')) $ lines contents
    let gameSet = map buildGame usefulData
    let openings = map (List.intercalate " " . fst) gameSet
    let openingDataList = zip openings $ map snd gameSet
    print $ length openingDataList
    let singletons = map (\(key, val) -> Trie.singleton key val) $ map createTrieNodeInfo openingDataList
    let trie = foldl1 (Trie.mergeBy (\(n1, w1) (n2, w2) -> Just (n1+n2, w1))) singletons
    --print $ Trie.size trie
    --let trie = trieFromListBy (\()) $ map createTrieNodeInfo openingDataList
    --print $ Trie.lookup (B.pack "e4 c5 c3 d5 exd5 Qxd5 d4 Nf6 dxc5 Qxd1+ Kxd1 e5 b4 Ne4 Bb5+ Bd7 Bxd7+ Nxd7 Ke2 O-O-O Nf3 f5 Bg5 Re8 Bd2 Be7 Na3 g5 Nc4 g4 Ne1 Rhg8 f3 gxf3+ gxf3 Nef6 Nc2 Rg2+ Kf1 Rg6 Rg1 Reg8 Kf2 f4 Rxg6 Rxg6 Rg1 Rh6 Rg2 Nd5 Ne1 Rh3 Nd3 e4 Nde5 e3+ Bxe3 fxe3+ Nxe3 Nxe3 Rg8+ Kc7 Nxd7 Kxd7 Kxe3 Rxh2 a4 Bf6 Kd3 Ra2 a5 Ra3 Rf8 Bxc3 Kc4 a6 Rf7+ Kc6 Rxh7 Bd2 Rf7 Ra4 Rf6+ Kc7 Rb6 Ra3 Rf6 Rc3+ Kd4 Rc1 Rf7+ Kc6 Rf6+ Kc7 Rf7+ Kb8 Rf8+ Ka7 Rf7 Bxb4 Rf5 Bxa5 c6 Bb6+ Ke4 Rxc6 Kd5 Rc5+ Ke6 Rxf5 Kxf5 a5 Ke4 Bc5 Kd3 b5 Kc2 Kb6") trie
    print $ Trie.match trie (B.pack "e5 c5")
    --print $ Map.size counter
    hClose handle
