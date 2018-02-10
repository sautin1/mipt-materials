--(email, name) = ("sautin@phystech.edu", encodeUtf8 "Саутин А.А. (2)")

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.IO as I -- to enable UTF-8
import System.Environment

type Sentence = [String]
type Character = String
type Relation = (String, String)

-- gets a list of relations in one sentence and generates a map from relation to counter
groupRelations :: [Relation] -> Map.Map (String, String) Integer
groupRelations = Map.fromList . map (\r -> (r, 1))

-- gets a list of related characters and generates a list of their relations
makeRelations :: [Character] -> [Relation]
makeRelations c = [ (x, y) | x <- c, y <- c, x < y]

splitText :: (Char -> Bool) -> String -> [String]
splitText p s  =  case dropWhile p s of
                  "" -> []
                  s' -> w : splitText p s''
                      where (w, s'') = break p s'

splitWords :: String -> [String]
splitWords = splitText (\x -> Char.isPunctuation x || Char.isSpace x)

-- gets a text and splits it into splitSentences
splitSentences :: String -> [Sentence]
splitSentences = (map splitWords) . splitText (flip elem ".!?")

-- checks if given string is a name of any character
isCharacter :: Set.Set String -> String -> Bool
isCharacter dict w
    | w == "" = False
    | otherwise = (Char.isUpper $ head w) && (Set.notMember (map Char.toLower w) dict)

-- runs through the sentence, returns list of Characters
characters :: Set.Set String -> Sentence -> [Character]
characters dict = filter (isCharacter dict)

main = do
    args <- getArgs
    dictData <- I.readFile $ args !! 0
    let dict = Set.fromList $ lines dictData
    contents <- I.getContents
    let charactersAll = map (characters dict) $ splitSentences contents
    let relationsAll = map (groupRelations.makeRelations) charactersAll -- list of maps from (Character, Character) to Integer
    let relationsCount = Map.unionsWith (+) relationsAll
    let relationsSorted = List.sortBy (\(_, a) (_, b) -> compare b a) $ Map.toList relationsCount
    --putStrLn "Characters:"
    --mapM_ print charactersAll
    --putStrLn ""
    --putStrLn "Relations:"
    mapM_ I.print relationsSorted
