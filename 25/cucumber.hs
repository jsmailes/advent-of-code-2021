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

data Cucumber = Empty | South | East deriving (Eq, Show)

get_data :: [String] -> [[Cucumber]]
get_data = map (map to_cucumber)

to_cucumber :: Char -> Cucumber
to_cucumber '.' = Empty
to_cucumber 'v' = South
to_cucumber '>' = East
to_cucumber _   = error "Couldn't parse cucumber"

from_cucumber :: Cucumber -> Char
from_cucumber Empty = '.'
from_cucumber South = 'v'
from_cucumber East  = '>'

from_data :: [[Cucumber]] -> String
from_data = unlines . map (map from_cucumber)


-- LOGIC --

neighbours :: [[a]] -> [[(a, a, a, a, a)]]
neighbours css = neighbours_col (map neighbours_row css)

-- (Left, Current, Right)
neighbours_row :: [a] -> [(a, a, a)]
neighbours_row cs = f (head cs) ((last cs):cs)
    where f a (x:y:z:xs) = (x,y,z):(f a (y:z:xs))
          f a (y:z:[])   = (y,z,a):[]

-- (Current, Left, Right, Up, Down)
neighbours_col :: [[(a, a, a)]] -> [[(a, a, a, a, a)]]
neighbours_col css = f (head css) ((last css):css)
    where f a (x:y:z:xs) = (g x y z):(f a (y:z:xs))
          f a (y:z:[])   = (g y z a):[]
          g ((_,a,_):as) ((b,c,d):bs) ((_,e,_):cs) = (c,b,d,a,e):(g as bs cs)
          g _ _ _ = []

update_east :: (Cucumber, Cucumber, Cucumber, Cucumber, Cucumber) -> Cucumber
update_east (Empty, East, _, _, _) = East
update_east (Empty, _, _, _, _) = Empty
update_east (East, _, Empty, _, _) = Empty
update_east (East, _, _, _, _) = East
update_east (South, _, _, _, _) = South

update_south :: (Cucumber, Cucumber, Cucumber, Cucumber, Cucumber) -> Cucumber
update_south (Empty, _, _, South, _) = South
update_south (Empty, _, _, _, _) = Empty
update_south (South, _, _, _, Empty) = Empty
update_south (South, _, _, _, _) = South
update_south (East, _, _, _, _) = East

update_step :: [[Cucumber]] -> [[Cucumber]]
update_step = map (map update_south) . neighbours . map (map update_east) . neighbours

num_steps :: [[Cucumber]] -> Int
num_steps = (+1) . length . takeWhile (not) . f . iterate update_step
    where f xs = zipWith (==) xs (tail xs)

test_data = get_data ["v...>>.vv>", ".vv>>.vv..", ">>.>v>...v", ">>v>>.>.v.", "v>v.vv.v..", ">.>>..v...", ".vv..>.>v.", "v.v..>>v.v", "....v..v.>"]


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let cucumbers = get_data strings
    let cucumbers' = update_step cucumbers
    let steps = num_steps cucumbers
    putStrLn $ show cucumbers
    putStrLn $ from_data cucumbers'
    putStrLn $ show steps
