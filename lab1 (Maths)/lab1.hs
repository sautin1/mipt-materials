{-# LANGUAGE OverloadedStrings #-}

module Lab1 (fTailor, tailor, tailorA, fSolve, iter, newton, dichotomy, fact) where

import System.IO
import System.Environment
import System.Directory
import Control.Monad
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Network (withSocketsDo)

-- почтовый адрес
email = "sautin@phystech.edu"
-- общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = (Double, Integer)

fTailor x = (exp x - exp (-x)) / 2 -- функция, которую раскладываем
delta = 1e-10
(n, a, b) = (20, 0, 1) -- интервал

-- faster mode
tailor :: Double -> Result
tailor x = tailorStep 0 1 1
	where
		tailorStep sum n prev
			| abs nthMember < delta = (sum, n)
			| otherwise = tailorStep (sum + nthMember) (n + 1) nthMember
			where nthMember = nth n prev
		nth 1 _ = x
		nth n prev = prev * (x ^ 2) / (fromIntegral ((2*n - 1) * (2*n - 2)))

fact n = foldl (*) 1 [1..n]

-- slower mode
tailorA :: Double -> Result
tailorA x = tailorStep 0 1
	where 
		tailorStep sum n
			| abs nthMember < delta = (sum, n)
			| otherwise = tailorStep (sum + nthMember) (n + 1)
			where nthMember = nth n
		nth n =	(x ^ (fromIntegral (2*n - 1))) / (fact (fromIntegral (2*n - 1)))

printTailor = mapM_ putStrLn $ 
	map 
		(\ x -> 
			let ((firstRes, firstCou), (secondRes, secondCou)) = (tailor x, tailorA x) 
			in show x ++ "\t" ++ show firstRes ++ "\t" ++ show firstCou ++ "\t" ++ show secondRes ++ "\t" ++ show secondCou ++ "\t" ++ show (fTailor x)) 
		[a, a + (b - a) / n .. b]

-- *** Вторая часть
fSolve = [
	\x -> x + cos (x ** 0.52 + 2),
	\x -> 3 * (log x ^ 2) + 6 * (log x) - 5,
	\x -> 0.6 * 3**x - 2.3 * x - 3
	] -- functions to solve

intervals = [
	(0.5, 1),
	(1, 3),
	(2, 3)
	] -- intervals

derivative :: (Double -> Double) -> Double -> Double
derivative f x = (f (x + delta) - (f x)) / delta

iter :: (Double -> Double) -> Double -> Double -> Result
iter f a b = iter' a 1
	where
		iter' x n
			| abs (x - nextX) < delta = (x, n)
			| otherwise = iter' nextX (n + 1)
			where 
				nextX = x - (f x) / (derivative f a)

newton :: (Double -> Double) -> Double -> Double -> Result
newton f a b = newton' ((a+b) / 2) 1
	where
		newton' x n
			| abs (x - nextX) < delta = (x, n)
			| otherwise = newton' nextX (n + 1)
			where nextX = x - (f x) / (derivative f x)

dichotomy :: (Double -> Double) -> Double -> Double -> Result
dichotomy = dichotomyStep 0
	where 
		dichotomyStep i f a b
			| b - a <= delta = (mid, i + 1)
			| signum (f a) == signum (f mid) = dichotomyStep (i + 1) f mid b
			| otherwise = dichotomyStep (i + 1) f a mid
			where mid = (b + a) / 2


printSolve = mapM_ putStrLn $ map show results
	where 
		results = [meth f (fst ab) (snd ab) | (f, ab) <- zip fSolve intervals, meth <- methods]
		methods = [iter, newton, dichotomy]


main = do	
	withSocketsDo $ do
	dir <- getCurrentDirectory
	initReq <- parseUrl "http://mipt.eu01.aws.af.cm/lab1"
	handle <- openFile (dir ++ "/lab1.hs") ReadMode
	hSetEncoding handle utf8_bom
	content <- hGetContents handle
	let req = urlEncodedBody [("email", email), ("content", C.pack content)] $ initReq { method = "POST" }
	response <- withManager $ httpLbs req
	hClose handle
	L.putStrLn $ responseBody response
