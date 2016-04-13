--https://www.codeeval.com/open_challenges/7/submit/

import System.Environment 	(getArgs)
import Data.List.Split 		(splitOn)
import Data.Maybe

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	let tests0 = splitOn "\n"  input
	let tests = remove_empty_lists tests0
	--putStrLn (show tests)
	mapM_ (putStrLn.show) (map process_test tests)

process_test input = 
	get_result (remove_empty_lists (splitOn " " input)) []

get_result :: [String] -> [(String, Maybe Int, Maybe Int)] -> Int
get_result [n2] [(op, Just n1, Nothing)] = 
	calculate_result op n1 (read n2 :: Int)
get_result (h:t) acc
	| or (map (==h) ["+", "*", "/"]) =
		get_result t ((h, Nothing, Nothing):acc)
	| otherwise = 
		get_result t (push_value (read h :: Int) acc)


push_value :: Int -> [(String, Maybe Int, Maybe Int)] -> [(String, Maybe Int, Maybe Int)]
push_value n ((op, Nothing, Nothing):t) = 
	(op, Just n, Nothing):t
push_value n2 ((op, Just n1, Nothing):t) = 
	push_value (calculate_result op n1 n2) t

calculate_result :: String -> Int -> Int -> Int
calculate_result "+" n1 n2 = 
	n1 + n2
calculate_result "*" n1 n2 = 
	n1 * n2
calculate_result "/" n1 n2 = 
	quot n1 n2

remove_empty_lists ("":t) = 
	remove_empty_lists t
remove_empty_lists (h:t) = 
	h:(remove_empty_lists t)
remove_empty_lists [] = 
	[]


