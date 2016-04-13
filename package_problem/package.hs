--https://www.codeeval.com/open_challenges/114/

import System.Environment 	(getArgs)
import Data.List.Split 		(splitOn)
import Data.List 			(sortBy, subsequences)
import Data.Ord


main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	let tests = remove_empty_lists $ splitOn "\n"  input
	--putStrLn (show tests)
	mapM_ (putStrLn.showListInt) (map process_test tests)

process_test input = 
	let 
		(weight_pack, items) = read_info input []
		valid_subseqs = 
			concat
				[
					let 
						sel_items = [items!!i | i <- is ]
						iden_items = [iden | (iden, _, _) <- sel_items]
						weight_items = sum [weight | (_, weight, _) <- sel_items]
						value_items = sum [value | (_, _, value) <- sel_items]
					in 
						case weight_items > (fromIntegral weight_pack) of 
							True -> 
								[]
							False ->
								[(iden_items, weight_items, value_items)]
					| is <- subsequences [0..((length items)-1)]
				]
		(iden, _, _) = head (sortBy ordering_fun valid_subseqs)
		--search_best_combination package items 
	in 
		iden
		--head $ sortBy ordering_fun valid_subseqs 
	--get_result (remove_empty_lists (splitOn " " input)) []

ordering_fun (_, w1, v1) (_, w2, v2) 
	| v1 > v2 = LT
	| (v1 == v2) && (w1 < w2) = LT
	| otherwise = GT

read_info (':':t) acc = 
	((read (reverse acc)) :: Int, read_items t)
read_info (h:t) acc = 
	read_info t (h:acc)

read_items list0 = 
	let
		list = filter (/='$') list0 
		items_str = remove_empty_lists $ splitOn " "  list
	in 
		map (\s -> read s::(Int, Float, Int)) items_str

remove_empty_lists ("":t) = 
	remove_empty_lists t
remove_empty_lists (h:t) = 
	h:(remove_empty_lists t)
remove_empty_lists [] = 
	[]

showListInt [] = 
	"-"
showListInt [a] = 
	(show a)
showListInt (h:t) = 
	(show h) ++ "," ++ (showListInt t)


