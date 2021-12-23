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

type Grid = [[Int]]
type State = [[Maybe Int]]

get_data :: [String] -> Grid
get_data lines = map (map (read . (:[]))) lines


-- LOGIC --

init_state :: Grid -> State
init_state grid = [ [ if x == 1 && y == 1 then Just 0 else Nothing | x <- [1..length (head grid)] ] | y <- [1..length grid] ]
--init_state grid = [ [ if x == 1 && y == 1 then Just ((grid!!0)!!0) else Nothing | x <- [1..length (head grid)] ] | y <- [1..length grid] ]

-- Returns all neighbours of i, j where state is not Nothing
neighbours :: State -> Int -> Int -> Int -> Int -> [Maybe Int]
--neighbours state x y x_max y_max = [ (state!!(y+b))!!(x+a) | a <- [-1,0], b <- [-1,0], x+a >= 0, x+a < x_max, y+b >= 0, y+b < y_max, not (a == 0 && b == 0), not (a /= 0 && b /= 0), isJust ((state!!(y+b))!!(x+a)) ]
neighbours state x y x_max y_max = [ (state!!(y+b))!!(x+a) | a <- [-1..1], b <- [-1..1], x+a >= 0, x+a < x_max, y+b >= 0, y+b < y_max, not (a == 0 && b == 0), not (a /= 0 && b /= 0), isJust ((state!!(y+b))!!(x+a)) ]

update_cell :: Grid -> State -> Int -> Int -> Maybe Int
update_cell grid state x y
  | length ns == 4                                        = (state!!y)!!x
  | isNothing ((state!!y)!!x) && length ns == 0           = Nothing
  | isNothing ((state!!y)!!x) && length ns /= 0           = fmap (+((grid!!y)!!x)) (minimum ns)
  | isJust ((state!!y)!!x)    && length ns == 0           = (state!!y)!!x
  | ((state!!y)!!x) < fmap (+((grid!!y)!!x)) (minimum ns) = (state!!y)!!x
  | otherwise                                             = fmap (+((grid!!y)!!x)) (minimum ns)
        where ns = neighbours state x y (length (head grid)) (length grid)

is_complete :: State -> Bool
is_complete = all (all isJust)

update_state :: Grid -> State -> State
update_state grid state = [ [ update_cell grid state (x-1) (y-1) | x <- [1..length (head grid)] ] | y <- [1..length grid] ]

run_system :: Grid -> State -> State
run_system grid state
  | is_complete state = state
  | otherwise = run_system grid (update_state grid state)

result :: State -> Int
result = fromJust . last . last

big_grid :: Grid -> Grid
big_grid grid = [ [ (mod (elem + a + b - 1) 9) + 1 | b <- [0..4], elem <- row ] | a <- [0..4], row <- grid ]


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let grid = get_data strings
    let state = init_state grid
    let state' = run_system grid state
    let cost = result state'
    let grid_big = big_grid grid
    let state_big = init_state grid_big
    let state_big' = run_system grid_big state_big
    let cost_big = result state_big'
    putStrLn $ show grid
    --putStrLn $ show state
    --putStrLn $ show state'
    putStrLn $ show cost
    putStrLn $ show cost_big
