import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split
import Data.Maybe


-- PARSING --

get_data :: [[Char]] -> [[Int]]
get_data lines = map (map (read . (:[]))) lines


-- LOGIC --

increment_all :: [[Int]] -> [[Int]]
increment_all = map (map (+1))

-- potential approach: flood fill
-- keep a stack of squares that need to be incremented
-- push new cells when they go above 10

initial_steps :: [(Int, Int)]
initial_steps = [ (x, y) | x <- [0..9], y <- [0..9] ]

neighbours :: Int -> Int -> [(Int, Int)]
neighbours x y = [ (x + a, y + b) | a <- [-1..1], b <- [-1..1], in_range (x + a) (y + b) ]
    where in_range x' y' = (x' >= 0) && (x' <= 9) && (y' >= 0) && (y' <= 9) && (not ((x == x') && (y == y')))

process_step :: [[Int]] -> [(Int, Int)] -> [[Int]]
process_step oss [] = oss
process_step oss ((x, y):cs) = process_step oss' cs'
    where oss' = modify_grid oss x y o'
          cs' = (if o' == 10 then neighbours x y else []) ++ cs
          o' = ((oss!!y)!!x) + 1

modify_grid :: [[Int]] -> Int -> Int -> Int -> [[Int]]
modify_grid [] _ _ _ = error "Ran out of rows"
modify_grid (os:oss) x y o'
  | y == 0    = (modify_row os x o'):oss
  | otherwise = os:(modify_grid oss x (y-1) o')

modify_row :: [Int] -> Int -> Int -> [Int]
modify_row [] _ _ = error "Ran out of columns"
modify_row (o:os) x o'
  | x == 0    = o':os
  | otherwise = o:(modify_row os (x-1) o')

reset_and_count :: [[Int]] -> ([[Int]], Int)
reset_and_count [] = ([], 0)
reset_and_count (os:oss) = (os':oss', c + c')
    where (os', c) = reset_count_row os
          (oss', c') = reset_and_count oss

reset_count_row :: [Int] -> ([Int], Int)
reset_count_row [] = ([], 0)
reset_count_row (o:os)
    | o > 9     = (0:os', c+1)
    | otherwise = (o:os', c)
        where (os', c) = reset_count_row os

advance_step :: [[Int]] -> ([[Int]], Int)
advance_step oss = reset_and_count oss'
    where oss' = process_step oss initial_steps

advance_steps :: [[Int]] -> Int -> ([[Int]], Int)
advance_steps oss 0 = (oss, 0)
advance_steps oss n = (oss'', c + c')
    where (oss', c) = advance_step oss
          (oss'', c') = advance_steps oss' (n-1)

all_flash :: [[Int]] -> Bool
all_flash oss = all (all (==0)) oss

first_all_flash :: [[Int]] -> Int
first_all_flash oss
    | all_flash oss = 0
    | otherwise     = 1 + first_all_flash oss'
        where (oss', _) = advance_step oss


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let octos = get_data strings
    let (octos', count) = advance_steps octos 100
    let first = first_all_flash octos
    putStrLn $ show octos
    putStrLn $ show octos'
    putStrLn $ show count
    putStrLn $ show first
