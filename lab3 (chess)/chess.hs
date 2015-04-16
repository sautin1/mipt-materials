--(email, name) = ("sautin@phystech.edu", encodeUtf8 "Саутин А.А. (2)")

import System.IO
import System.Environment
import qualified Data.Char as Char
import qualified Data.Map as Map

type Turn = String
type Game = ([Turn], Bool) -- (turns, whiteWins)
type Opening = String -- concatenation of turns-strings intercalated by space

buildGame :: String -> Game
buildGame s = (turnsString, gameResult)
    where
        (rawTurns, rawResult) = break (\x -> head x == '{') $ words s
        turnsString = filter (\x -> Char.isLetter $ head x) rawTurns
        gameResult = head (last rawResult) == '1'

generateOpenings :: Game -> [Opening]
generateOpenings = (scanl1 (\x y -> x ++ (' ':y)) . fst)
-- without lambda: flip ((flip (++)).(' ':))

createOpeningCounter :: [Opening] -> Map.Map Opening Integer
createOpeningCounter = Map.fromList . map (\x -> (x, 1))

main = do
    args <- getArgs
    handle <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle
    let usefulData = filter (\s -> (s /= "\r") && (head s /= '[')) $ lines contents
    let gameSet = map buildGame usefulData
    let inCounter = map (createOpeningCounter . generateOpenings) gameSet
    --print $ length inCounter
    --putStrLn "___"
    mapM_ print inCounter
    let counter = Map.filter (> 1) $ Map.unionsWith (+) inCounter
    print $ Map.size counter
    --mapM_ print $ take 10 $ Map.toList counter
    hClose handle
