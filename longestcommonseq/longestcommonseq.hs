--https://www.codeeval.com/open_challenges/6/

import System.Environment 	(getArgs)
import Data.List.Split 		(splitOn)
import Data.Ord          	( comparing )
import Data.List         	( maximumBy, subsequences )
import Debug.Trace

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	--putStrLn input
	-- print your output to stdout
	let tests = splitOn "\n"  input
	--let maxim = 
	--	maximumBy 
	--		(comparing length)
	--		(search_longest_sseq_tests tests)
			--[search_longest_sseq (drop i (ws!!0)) (ws!!1) [] 
			-- | i <- [0..( (length (ws!!0)) - 1) ] ] 
			--[w1 | 
			--	w1 <- (subsequences (ws!!0)), 
			--	w2 <- (subsequences (ws!!1)), 
			--	w1 == w2]
	--putStrLn (show tests)
	--putStrLn (show $ reverse maxim)
	--let maxim2 = 
	--	maximumBy 
	--		(comparing length)
	--		[w1 | 
	--			w1 <- (subsequences (ws!!0)), 
	--			w2 <- (subsequences (ws!!1)), 
	--			w1 == w2]
	--putStrLn (show $ maxim2)
	mapM_ (putStrLn.reverse) $ (search_longest_sseq_tests tests)

search_longest_sseq_tests tests = 
	map 
		(\test -> 
			let 
				ws = splitOn ";"  test
			in
				maximumBy 
					(comparing length) 
					(sseq (remove_white (ws!!0)) (remove_white (ws!!1))  [])
					--(trace (show ((sseq (remove_white (ws!!0)) (remove_white (ws!!1))  []))) (sseq (ws!!0) (ws!!1)  []) ) )
		)
		tests

sseq []  _  current = 
	[current]
sseq _  []  current = 
	[current]
sseq (h:t) currentl2 current  = 
	case dropWhile (/= h) currentl2 of 
		[] ->
			sseq t currentl2 current
		(h:nl2) ->
				sseq t nl2 (h:current)
			++  sseq t currentl2 current

remove_white [] = []
remove_white (' ':a) = remove_white a
remove_white (a:b) = a : remove_white b