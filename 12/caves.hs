import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char


-- PARSING --

data Node = Start | End | Small String | Large String deriving (Show, Eq)
type Edge = (Node, Node)
type Path = [Node]

get_data :: [String] -> [Edge]
get_data lines = map to_edge lines

to_edge :: String -> Edge
to_edge line = (to_node (ls!!0), to_node (ls!!1))
    where ls = splitOn "-" line

to_node :: String -> Node
to_node "start" = Start
to_node "end" = End
to_node cs
  | all_upper cs = Large cs
  | all_lower cs = Small cs
  | otherwise = error "Unable to infer cave size"

all_upper :: String -> Bool
all_upper = all isUpper

all_lower :: String -> Bool
all_lower = all isLower


-- LOGIC --

path_done :: Path -> Bool
path_done = (==End) . head

matching_edges :: Node -> [Edge] -> [Node]
matching_edges n es = [ v | (u, v) <- es, n == u ] ++ [ u | (u, v) <- es, n == v ]

extend_path :: Path -> [Edge] -> [Path]
extend_path [] _      = error "Path is empty"
extend_path (n:ns) es = [ (u:n:ns) | u <- matching_edges n es, check_path_conditions u (n:ns) ]

check_path_conditions :: Node -> Path -> Bool
check_path_conditions End _ = True
check_path_conditions Start _ = False
check_path_conditions (Large _) _ = True
check_path_conditions (Small c) ns = not (any (==(Small c)) ns)

all_paths :: [Edge] -> [Path]
all_paths es = process_paths es [[Start]]

process_paths :: [Edge] -> [Path] -> [Path]
process_paths es [] = []
process_paths es (p:ps)
  | path_done p = p:(process_paths es ps)
  | otherwise   = process_paths es (ps' ++ ps)
        where ps' = extend_path p es


-- LOGIC, PART 2 --

extend_path_new :: Path -> [Edge] -> [Path]
extend_path_new [] _      = error "Path is empty"
extend_path_new (n:ns) es = [ (u:n:ns) | u <- matching_edges n es, path_conditions_new u (n:ns) ]

path_conditions_new :: Node -> Path -> Bool
path_conditions_new End _ = True
path_conditions_new Start _ = False
path_conditions_new (Large _) _ = True
path_conditions_new (Small c) ns = (a == 0) || ((a == 1) && no_dups (small_nodes ns))
    where a = length (filter (==(Small c)) ns)

small_nodes :: Path -> [String]
small_nodes = sort . map f . filter is_small
    where is_small (Small _) = True
          is_small _         = False
          f (Small a) = a
          f _         = error "This shouldn't happen"

no_dups :: [String] -> Bool
no_dups (x:y:xs) = if x == y then False else no_dups (y:xs)
no_dups _ = True

all_paths_new :: [Edge] -> [Path]
all_paths_new es = process_paths_new es [[Start]]

process_paths_new :: [Edge] -> [Path] -> [Path]
process_paths_new es [] = []
process_paths_new es (p:ps)
  | path_done p = p:(process_paths_new es ps)
  | otherwise   = process_paths_new es (ps' ++ ps)
        where ps' = extend_path_new p es


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let edges = get_data strings
    let paths = all_paths edges
    let num_paths = length paths
    let paths_new = all_paths_new edges
    let num_paths_new = length paths_new
    putStrLn $ show edges
    putStrLn $ show num_paths
    putStrLn $ show num_paths_new
