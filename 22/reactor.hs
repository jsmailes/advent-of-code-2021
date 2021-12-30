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

-- on/off, x_min, x_max, y_min, y_max, z_min, z_max
type Step = (Bool, Int, Int, Int, Int, Int, Int)

get_data :: [String] -> [Step]
get_data lines = map get_step lines

get_step :: String -> Step
get_step ('o':'n':' ':cs)     = get_coords True cs
get_step ('o':'f':'f':' ':cs) = get_coords False cs
get_step _                    = error "Step not recognised"

get_coords :: Bool -> String -> Step
get_coords b cs = (b, x_min, x_max, y_min, y_max, z_min, z_max)
    where (x_min, x_max) = get_coord $ drop 2 $ cs' !! 0
          (y_min, y_max) = get_coord $ drop 2 $ cs' !! 1
          (z_min, z_max) = get_coord $ drop 2 $ cs' !! 2
          cs' = splitOn "," cs

get_coord :: String -> (Int, Int)
get_coord cs = (read (cs'!!0), read (cs'!!1))
    where cs' = splitOn ".." cs


-- LOGIC --

apply_step :: Bool -> Int -> Int -> Int -> Step -> Bool
apply_step b x y z (b', x_min, x_max, y_min, y_max, z_min, z_max) = if in_range then b' else b
    where in_range = x >= x_min && x <= x_max && y >= y_min && y <= y_max && z >= z_min && z <= z_max

apply_steps :: Bool -> Int -> Int -> Int -> [Step] -> Bool
apply_steps b x y z steps = foldl (\b' s -> apply_step b' x y z s) b steps

initialisation :: [Step] -> Int
initialisation steps = sum [ fromEnum (apply_steps False x y z steps) | x <- [-50..50], y <- [-50..50], z <- [-50..50] ]


-- LOGIC PART 2 --

-- Okay so what we do here is figure out the volume of each "on" step, then pairwise figure out the volume of each overlap
-- If "on" overlaps with "on", subtract the overlap
-- If "on" overlaps with "off", subtract the overlap
-- If "off" overlaps with "off", do nothing
-- Note: only works if no more than two regions overlap at a single point...

-- New approach: keep list of "on" rectangles
-- If "on" or "off" overlaps with a rectangle, split
-- Could do as a tree structure?

type Range = (Int, Int, Int, Int, Int, Int)
data Tree = Node Range [Tree] | Leaf Range Bool deriving (Show)

init_range :: [Step] -> Range
init_range steps = (xmin, xmax, ymin, ymax, zmin, zmax)
    where xmin = minimum $ map (\(_,a,_,_,_,_,_) -> a) steps
          xmax = maximum $ map (\(_,_,a,_,_,_,_) -> a) steps
          ymin = minimum $ map (\(_,_,_,a,_,_,_) -> a) steps
          ymax = maximum $ map (\(_,_,_,_,a,_,_) -> a) steps
          zmin = minimum $ map (\(_,_,_,_,_,a,_) -> a) steps
          zmax = maximum $ map (\(_,_,_,_,_,_,a) -> a) steps

init_tree :: Range -> Tree
init_tree range = Leaf range False

range_overlaps :: Range -> Range -> Bool
range_overlaps range range' = (overlaps xmin xmax xmin' xmax') && (overlaps ymin ymax ymin' ymax') && (overlaps zmin zmax zmin' zmax')
    where (xmin,  xmax,  ymin,  ymax,  zmin,  zmax)  = range
          (xmin', xmax', ymin', ymax', zmin', zmax') = range'
          overlaps min max min' max' = min <= max' && min' <= max

update_tree :: Range -> Bool -> Tree -> Tree
update_tree range' state' (Leaf range state)
  | range == range'             = Leaf range' state'
  | range_overlaps range range' = Node range (split_tree range range' state state')
  | otherwise                   = Leaf range state
update_tree range' state' (Node range trees)
  | range == range'             = Leaf range' state'
  | range_overlaps range range' = Node range (map (update_tree range' state') trees)
  | otherwise                   = Node range trees

update_tree_step :: Tree -> Step -> Tree
update_tree_step t (state, xa, xb, ya, yb, za, zb) = update_tree (xa, xb, ya, yb, za, zb) state t

update_tree_steps :: Tree -> [Step] -> Tree
update_tree_steps = foldl update_tree_step

split_tree :: Range -> Range -> Bool -> Bool -> [Tree]
split_tree range range' state state' = filter valid ((Leaf main_range state'):(map (\r -> Leaf r state) other_ranges))
    where (xmin,   xmax,   ymin,   ymax,   zmin,   zmax)   = range
          (xmin',  xmax',  ymin',  ymax',  zmin',  zmax')  = range'
          (xmin'', xmax'', ymin'', ymax'', zmin'', zmax'') = (max xmin xmin', min xmax xmax', max ymin ymin', min ymax ymax', max zmin zmin', min zmax zmax')
          main_range = (xmin'', xmax'', ymin'', ymax'', zmin'', zmax'')
          other_ranges = [(xmin, xmin'' - 1, ymin, ymax, zmin, zmax), (xmax'' + 1, xmax, ymin, ymax, zmin, zmax), (xmin'', xmax'', ymin, ymin'' - 1, zmin, zmax), (xmin'', xmax'', ymax'' + 1, ymax, zmin, zmax), (xmin'', xmax'', ymin'', ymax'', zmin, zmin'' - 1), (xmin'', xmax'', ymin'', ymax'', zmax'' + 1, zmax)]
          valid (Leaf (xa, xb, ya, yb, za, zb) _) = xa <= xb && ya <= yb && za <= zb

volume :: Range -> Int
volume (xa, xb, ya, yb, za, zb) = x * y * z
    where x = abs $ xb - xa + 1
          y = abs $ yb - ya + 1
          z = abs $ zb - za + 1

count_on :: Tree -> Int
count_on (Leaf range state) = if state then volume range else 0
count_on (Node _ trees) = sum $ map count_on trees


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let steps = get_data strings
    let count = initialisation (take 20 steps)
    let tree = init_tree $ init_range steps
    let count' = count_on $ update_tree_steps tree (take 20 steps)
    let count'' = count_on $ update_tree_steps tree steps
    putStrLn $ show steps
    putStrLn $ show count
    putStrLn $ show count'
    putStrLn $ show count''
