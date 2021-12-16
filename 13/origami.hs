import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char


-- PARSING --

type Coordinate = (Int, Int)
data Fold = X Int | Y Int deriving (Eq, Show)

get_data :: [String] -> ([Coordinate], [Fold])
get_data lines = (map to_coordinate (lines_split!!0), map to_fold (lines_split!!1))
    where lines_split = splitOn [""] lines

to_coordinate :: String -> Coordinate
to_coordinate cs = (cs'!!0, cs'!!1)
    where cs' = map read (splitOn "," cs)

to_fold :: String -> Fold
to_fold cs
  | (cs'!!0) == "fold along x" = X (read (cs'!!1))
  | otherwise                  = Y (read (cs'!!1))
        where cs' = splitOn "=" cs


-- LOGIC --

-- Okay so what we're gonna do here is take the list of coordinates, take a fold, and modify the coordinates using a map
-- Then we sort and remove duplicates

fold_coord :: Fold -> Coordinate -> Coordinate
fold_coord (X fx) (x, y)
  | x < fx    = (x,             y)
  | otherwise = (fx - (x - fx), y)
fold_coord (Y fy) (x, y)
  | y < fy    = (x, y)
  | otherwise = (x, fy - (y - fy))

fold_coords :: Fold -> [Coordinate] -> [Coordinate]
fold_coords f = remove_dups . sort . map (fold_coord f)

remove_dups :: Eq a => [a] -> [a]
remove_dups (x:y:xs) = if x == y then remove_dups (x:xs) else x:(remove_dups (y:xs))
remove_dups xs       = xs

fold_all :: [Fold] -> [Coordinate] -> [Coordinate]
fold_all fs cs = foldl (flip fold_coords) cs fs

draw_coords :: [Coordinate] -> Int -> Int -> [String]
draw_coords cs max_x max_y = [ [ draw_coord cs (x,y) | x <- [0..max_x] ] | y <- [0..max_y] ]

draw_coord :: [Coordinate] -> Coordinate -> Char
draw_coord cs c = if ((>0) . length . filter (==c)) cs then '#' else '.'

main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let (coords, folds) = get_data strings
    let coords' = fold_coords (head folds) coords
    let coords_final = fold_all folds coords
    let coords_shown = draw_coords coords_final 38 5
    putStrLn $ show coords
    putStrLn $ show folds
    putStrLn $ show coords'
    putStrLn $ show (length coords')
    putStrLn $ show coords_final
    putStrLn $ unlines coords_shown
