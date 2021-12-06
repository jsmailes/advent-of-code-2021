import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split


-- PARSING --

type Field = [[Int]]
type Line = (Int, Int, Int, Int)
type Point = (Int, Int)

get_data :: [[Char]] -> [Line]
get_data = map get_line

get_line :: [Char] -> Line
get_line = to_tuple . map read . concatMap (splitOn ",") . splitOn " -> "
    where to_tuple (a:b:c:d:_) = (a,b,c,d)

get :: Int -> a -> [a]
get n x = (take n . repeat) x

init_state :: Field
init_state = get 1000 (get 1000 0)

-- LOGIC --

process_lines :: [Line] -> [Point]
process_lines = concatMap process_line

process_line :: Line -> [Point]
process_line (x0, y0, x1, y1)
  | x0 == x1  = process_vline x0 (min y0 y1) (max y0 y1)
  | y0 == y1  = process_hline y0 (min x0 x1) (max x0 x1)
  | otherwise = []

process_lines_new :: [Line] -> [Point]
process_lines_new = concatMap process_line_new

process_line_new :: Line -> [Point]
process_line_new (x0, y0, x1, y1)
  | x0 == x1           = process_vline x0 (min y0 y1) (max y0 y1)
  | y0 == y1           = process_hline y0 (min x0 x1) (max x0 x1)
  | otherwise          = process_diag  x0 x1 y0 y1

process_vline :: Int -> Int -> Int -> [Point]
process_vline x y0 y1 = [(x, y) | y <- [y0..y1]]

process_hline :: Int -> Int -> Int -> [Point]
process_hline y x0 x1 = [(x, y) | x <- [x0..x1]]

process_diag :: Int -> Int -> Int -> Int -> [Point]
process_diag xa xb ya yb
  | xa > xb && ya > yb = process_diag_down x0 x1 y0 y1
  | xa < xb && ya < yb = process_diag_down x0 x1 y0 y1
  | otherwise          = process_diag_up   x0 x1 y0 y1
    where x0 = min xa xb
          x1 = max xa xb
          y0 = min ya yb
          y1 = max ya yb

process_diag_down :: Int -> Int -> Int -> Int -> [Point]
process_diag_down x0 x1 y0 y1 = [(x0+a, y0+a) | a <- [0..x1-x0]]

process_diag_up :: Int -> Int -> Int -> Int -> [Point]
process_diag_up x0 x1 y0 y1 = [(x0+a, y1-a) | a <- [0..x1-x0]]

apply_points :: [Point] -> Field -> Field
apply_points points field = map (apply_row points) (zip field [0..])

apply_row :: [Point] -> ([Int], Int) -> [Int]
apply_row ps (row, i) = foldl apply_point row qs
    where qs = map snd (filter ((==i) . fst) ps)
          apply_point :: [Int] -> Int -> [Int]
          apply_point []     _ = []
          apply_point (x:xs) 0 = (x+1):xs
          apply_point (x:xs) a = x:(apply_point xs (a-1))

danger :: Field -> Int
danger = sum . map danger_row
    where danger_row = sum . map (\x -> if x > 1 then 1 else 0)

main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let lines = get_data strings
    let points = process_lines lines
    let state = apply_points points init_state
    let total_danger = danger state
    let points_new = process_lines_new lines
    let state_new = apply_points points_new init_state
    let total_danger_new = danger state_new
    putStrLn $ show lines
    putStrLn $ show (length points)
    putStrLn $ show total_danger
    putStrLn $ show (length points_new)
    putStrLn $ show total_danger_new
