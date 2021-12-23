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

-- x, y, vel_x, vel_y
type State = (Int, Int, Int, Int)

-- x_min, x_max, y_min, y_max
type Bounds = (Int, Int, Int, Int)


-- LOGIC --

-- probe has gone too far
out_of_bounds :: Bounds -> State -> Bool
out_of_bounds (_, x_max, y_min, _) (x, y, _, _) = (x > x_max) || (y < y_min)

-- probe has landed in bounding box
in_bounds :: Bounds -> State -> Bool
in_bounds (x_min, x_max, y_min, y_max) (x, y, _, _) = (x >= x_min) && (x <= x_max) && (y >= y_min) && (y <= y_max)

-- update state by 1 step
update :: State -> State
update (x, y, vel_x, vel_y) = (x + vel_x, y + vel_y, vel_x', vel_y - 1)
    where vel_x'
            | vel_x > 0 = vel_x - 1
            | vel_x < 0 = vel_x + 1
            | otherwise = 0

repeat_f :: (a -> a) -> a -> [a]
repeat_f f x = x:(map f (repeat_f f x))

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

expand_state :: Bounds -> State -> ([State], Bool)
expand_state bounds state = (states, (in_bounds bounds . last) states)
    where states = (takeWhileInclusive (\s -> (not (in_bounds bounds s)) && (not (out_of_bounds bounds s))) . repeat_f update) state

max_height :: [State] -> Int
max_height states = (maximum . map second) states
    where second (_, y, _, _) = y

run_system :: Bounds -> Int -> Int -> Maybe Int
run_system bounds vel_x vel_y = if good then Just (max_height states) else Nothing
    where (states, good) = expand_state bounds (0, 0, vel_x, vel_y)

good_inits :: Bounds -> Int -> Int -> Int -> Int -> [(Int, Int)]
good_inits bounds x_min x_max y_min y_max = [ (x, y) | x <- [x_min..x_max], y <- [y_min..y_max], isJust (run_system bounds x y) ]

maximum_height :: Bounds -> [(Int, Int)] -> Int
maximum_height bounds = maximum . map (fromJust . (uncurry (run_system bounds)))


main = do
    let bounds = (124, 174, -123, -86)
    let goods = good_inits bounds 0 174 (-123) 200
    let h = maximum_height bounds goods
    putStrLn $ show bounds
    putStrLn $ show goods
    putStrLn $ show (length goods)
    putStrLn $ show h
