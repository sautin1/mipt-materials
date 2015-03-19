--import Data.Text.Encoding
--(email, name) = ("sautin@phystech.edu", encodeUtf8 "Саутин А.А. (2)") -- адрес почты и фамилия с инициалами

art =
	[
	"@..@..................",
	"..@..@................",
	"....@..@..@@@@@@@@@@@@",
	"......@..@............",
	"........@..@..........",
	"......@..@...@..@@@@@@",
	"....@..@.......@......",
	"..@..@...........@....",
	"@..@...............@.."
	]

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f x = (iterate f x !! n)

flipHor :: [[a]] -> [[a]]
flipHor [] = []
flipHor (x:xs) = (reverse x) : (flipHor xs)

flipVert :: [[a]] -> [[a]]
flipVert = reverse

rotate90 :: [[a]] -> [[a]]
rotate90 [] = []
rotate90 all@(x:xs) = foldl (flip consCol) (replicate (length x) []) all
	where
		consCol :: [a] -> [[a]] -> [[a]]
		consCol = zipWith (:)

rotate180 :: [[a]] -> [[a]]
rotate180 = applyNTimes 2 rotate90

rotate270 :: [[a]] -> [[a]]
rotate270 = applyNTimes 3 rotate90
