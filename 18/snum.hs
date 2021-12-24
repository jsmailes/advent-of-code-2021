import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Map (fromListWith, toList)
import Data.Tuple (swap)


-- PARSING --

data SNum = Pair SNum SNum | Val Int deriving (Show)

fold_snum :: (b -> b -> b) -> (Int -> b) -> SNum -> b
fold_snum f_pair f_val (Pair s1 s2) = f_pair (fold_snum f_pair f_val s1) (fold_snum f_pair f_val s2)
fold_snum f_pair f_val (Val i)      = f_val i

get_data :: [String] -> [SNum]
get_data lines = map to_snum lines

to_snum :: String -> SNum
to_snum s
  | head s == '[' = let (s1, s2) = split_toplevel_comma s in Pair (to_snum s1) (to_snum s2)
  | length s == 1 = Val (read s)
  | otherwise     = error ("Could not recognise " ++ s)

split_toplevel_comma :: String -> (String, String)
split_toplevel_comma s = split_level_comma 0 ((init . tail) s)

split_level_comma :: Int -> String -> (String, String)
split_level_comma 0 (',':s) = ([], s)
split_level_comma i ('[':s) = let (s', s'') = split_level_comma (i+1) s in ('[':s', s'')
split_level_comma i (']':s) = let (s', s'') = split_level_comma (i-1) s in (']':s', s'')
split_level_comma i (c:s)   = let (s', s'') = split_level_comma i     s in (  c:s', s'')
split_level_comma _ []      = error "Reached end of string"


-- LOGIC --

add :: SNum -> SNum -> SNum
add = Pair

-- Exploding is the difficult one
-- We want to do a left-to-right traversal, track depth, and reconstruct the tree afterwards
-- First step is to find the first explodable pair and its left-right neighbours
-- A depth-first traversal will naturally be left-to-right
-- So we have 3 states our function can be in:
-- - Searching for first deep-enough pair (tracking index)
-- - Find predecessor
-- - Find successor

explode_snum :: SNum -> SNum
explode_snum s = (fst . add_index (index+1) add_right . fst . add_index (index-1) add_left) s'
    where (s', result, index)   = traverse_snum 0 0 s
          (add_left, add_right) = fromJust result

-- Returns SNum with exploded pair set to 0, left element, right element, index
traverse_snum :: Int -> Int -> SNum -> (SNum, Maybe (Int, Int), Int)
traverse_snum depth index (Val i)                = (Val i, Nothing, index)
traverse_snum depth index (Pair (Val a) (Val b)) = if depth >= 4 then (Val 0, Just (a, b), index) else ((Pair (Val a) (Val b)), Nothing, index+1)
traverse_snum depth index (Pair s1 s2)           = if isJust r then (Pair s1' s2, r, i) else (Pair s1' s2', r', i')
    where (s1', r,  i)  = traverse_snum (depth+1) index s1
          (s2', r', i') = traverse_snum (depth+1) (i+1) s2

-- Traverses the SNum to the given index, adds the given value to its node
add_index :: Int -> Int -> SNum -> (SNum, Int)
add_index 0     value (Val i)      = (Val (i+value), 0)
add_index index value (Val i)      = (Val i, index)
add_index index value (Pair s1 s2) = if i <= 0 then (Pair s1' s2, i) else (Pair s1' s2', i')
    where (s1', i)  = add_index index value s1
          (s2', i') = add_index (i-1) value s2

split_snum :: SNum -> SNum
split_snum = fst . do_split

do_split :: SNum -> (SNum, Bool)
do_split (Val i)      = if i >= 10 then (Pair (Val d) (Val (d+m)), True) else (Val i, False)
    where (d, m) = divMod i 2
do_split (Pair s1 s2) = if r then (Pair s1' s2, r) else (Pair s1' s2', r')
    where (s1', r)  = do_split s1
          (s2', r') = do_split s2

can_explode :: SNum -> Bool
can_explode = (>4) . fold_snum (\x y -> (max x y) + 1) (const 0)

can_split :: SNum -> Bool
can_split = fold_snum (||) (>=10)

reduce :: SNum -> SNum
reduce s
  | can_explode s = (reduce . explode_snum) s
  | can_split s   = (reduce . split_snum)   s
  | otherwise     = s

magnitude :: SNum -> Int
magnitude = fold_snum (\l r -> (3*l) + (2*r)) id

largest_magnitude :: [SNum] -> Int
largest_magnitude = maximum . magnitudes
    where magnitudes snums = map magnitude [ reduce (add (snums!!i) (snums!!j)) | i <- [0..(length snums)-1], j <- [0..(length snums)-1], i /= j ]


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let snums = get_data strings
    let snum_final = foldl (\sa sb -> reduce (add sa sb)) (head snums) (tail snums)
    let snum_magnitude = magnitude snum_final
    let largest = largest_magnitude snums
    putStrLn $ show snums
    putStrLn $ show snum_final
    putStrLn $ show snum_magnitude
    putStrLn $ show largest
