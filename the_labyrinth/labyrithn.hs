--https://www.codeeval.com/open_challenges/7/submit/

import System.Environment 	(getArgs)
import Data.List.Split 		(splitOn)
import Data.Maybe
--import Data.Graph
import Data.Graph.Inductive.Query.SP
import Data.Graph.Inductive.Internal.RootPath
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.Heap

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	let tests0 = splitOn "\n"  input
	let tests = remove_empty_lists tests0
	--putStrLn (show tests)
	let result = process_test tests (length tests)
	--mapM_ (putStrLn) ()
	putStrLn (show result)

process_test :: [String] -> Int -> (gr0 String b0)
process_test test rows =
	let  
		cols = (length $ head test) 
		edges = read_graph test test 0 rows cols []
		--graph = 
		--shortest_path = sp 761 700 graph
	in 
		--graph
		--mkGraph [(v,(show v)) | v <- [1 .. rows*cols]] [(n1,n2,1) | (n1,n2) <- edges]
		Data.Graph.Inductive.Graph.empty

read_graph (row:t) matrix n_row rows cols acc = 
	let 
		nacc = read_graph_col row matrix n_row rows 0 cols acc 
	in 
		read_graph t matrix (n_row + 1) rows cols nacc 
read_graph [] _ _ _ _ acc = 
	acc

read_graph_col (h:t) matrix n_row rows n_col cols acc =
	let 
		acc1 = add_point (n_row > 0) ((matrix!!(n_row-1))!!(n_col)) (n_row * rows + n_col, (n_row - 1) * rows + n_col) acc
		acc2 = add_point (n_col > 0) ((matrix!!(n_row))!!(n_col-1)) (n_row * rows + n_col, n_row  * rows + n_col - 1) acc1
		acc3 = add_point (n_col < cols - 1) ((matrix!!(n_row))!!(n_col+1)) (n_row * rows + n_col, n_row  * rows + n_col + 1) acc2
		acc4 = 
			case (matrix!!n_row)!!n_col of 
				'*' ->
					acc 
				' ' ->
					add_point (n_row < rows - 1) ((matrix!!(n_row + 1))!!(n_col)) (n_row * rows + n_col, (n_row + 1)   * rows + n_col) acc3
	in 
		read_graph_col t matrix n_row rows (n_col + 1) cols acc4 
read_graph_col [] _ _ _ _ _ acc =
	acc

add_point cond pos edge acc = 
	case cond of 
		True ->
			case pos of 
				' ' ->
					(edge:acc)
				'*' ->
					acc
		False ->
			acc

remove_empty_lists ("":t) = 
	remove_empty_lists t
remove_empty_lists (h:t) = 
	h:(remove_empty_lists t)
remove_empty_lists [] = 
	[]

--spTreeM :: (Graph gr, Real b) => Node -> gr a b -> LRTree b
--spTreeM v = dijkstra (H.unit 0 (LP [(v,0)]))

--spath :: (Graph gr, Real b) => Node -> Node -> gr a b -> Path
--spath s t = getLPathNodes t . spTree s



