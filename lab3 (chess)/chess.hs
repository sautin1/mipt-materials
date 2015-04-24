--(email, name) = ("sautin@phystech.edu", encodeUtf8 "Саутин А.А. (2)")

import System.IO
import System.Environment
import qualified ChessTrie as Trie

main = do
    args <- getArgs
    handle <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle
    let usefulData = filter (\s -> (s /= "\r") && (head s /= '[')) $ lines contents -- list of useful strings
    let gameList = map Trie.buildGame usefulData -- list of games
    let trie = Trie.fromList gameList

    putStrLn "1. Longest opening (opening, length):"
    let longest = Trie.reverseResult $ Trie.traverse Trie.longestOpenVisit Trie.goDeeper ([], 0) ([], 0) trie
    print $ longest
    --print $ Trie.lookup (fst longest) trie

    putStrLn "\n2.1. Best white opening (opening, probability):"
    let bw = Trie.reverseResult $ Trie.traverse (Trie.winOpenWithLastVisit Trie.whiteTurn Trie.whiteProb) Trie.goDeeper 
        ([], 0.0) ([], 0) trie
    print $ bw
    --print $ Trie.lookup (fst bw) trie

    putStrLn "\n2.2. Best black opening (opening, probability):"
    let bb = Trie.reverseResult $ Trie.traverse (Trie.winOpenWithLastVisit Trie.blackTurn Trie.blackProb) Trie.goDeeper 
        ([], 0.0) ([], 0) trie
    print $ bb
    --print $ Trie.lookup (fst bb) trie

    putStrLn "\n3.1. Best opening for white (opening, probability):"
    let bfw = Trie.reverseResult $ Trie.traverse (Trie.winOpenVisit Trie.whiteProb) Trie.goDeeper ([], 0.0) ([], 0) trie
    print $ bfw
    --print $ Trie.lookup (fst bfw) trie

    putStrLn "\n3.2. Best opening for black (opening, probability):"
    let bfb = Trie.reverseResult $ Trie.traverse (Trie.winOpenVisit Trie.blackProb) Trie.goDeeper ([], 0.0) ([], 0) trie
    print $ bfb
    --print $ Trie.lookup (fst bfb) trie

    putStrLn "\n4.1. Worst white turn (opening, delta probability):"
    let ww = Trie.reverseResult $ Trie.traverse (Trie.worstTurnVisit Trie.whiteTurn Trie.whiteProb) (Trie.goDeeperProb Trie.whiteProb) 
        ([], -1.0) (([], 0), 0.0) trie
    print $ ww
    --print $ Trie.lookup (fst ww) trie

    putStrLn "\n4.2. Worst black turn (opening, delta probability):"
    let wb = Trie.reverseResult $ Trie.traverse (Trie.worstTurnVisit Trie.blackTurn Trie.blackProb) (Trie.goDeeperProb Trie.blackProb) 
        ([], -1.0) (([], 0), 0.0) trie
    print $ wb
    --print $ Trie.lookup (fst wb) trie

    hClose handle
