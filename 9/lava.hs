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

lowest :: Int -> Int -> Int -> Bool
lowest x y z = y < x && y < z

lowest_row :: [Int] -> [Bool]
lowest_row row = check_lowest ((10:row) ++ [10])

check_lowest :: [Int] -> [Bool]
check_lowest (x0:x1:x2:xs) = (lowest x0 x1 x2):(check_lowest (x1:x2:xs))
check_lowest _             = []

check_lava :: [[Int]] -> [[Bool]]
check_lava xss = zipWith f checked_rows checked_columns
    where checked_rows = map lowest_row xss
          checked_columns = transpose (map lowest_row (transpose xss))
          f a b = zipWith (&&) a b

risk :: [[Int]] -> [[Bool]] -> [[Int]]
risk lava lowest = zipWith (zipWith f) lava lowest
    where f height low = if low then 1 + height else 0

-- so we enumerate each lowest point and make those basins
-- then we grow each basin out iteratively according to the following rule:
-- for each square that is not 9 or currently in a basin, if all of its adjacent lower squares are in a basin then it is in a basin

enumerate_basins :: [[Bool]] -> [[Maybe Int]]
enumerate_basins = enumerate_basins_step 1

enumerate_basins_step :: Int -> [[Bool]] -> [[Maybe Int]]
enumerate_basins_step i [] = []
enumerate_basins_step i (bs:bss) = cs:(enumerate_basins_step j bss)
   where (cs, j) = enumerate_basins_row i bs

enumerate_basins_row :: Int -> [Bool] -> ([Maybe Int], Int)
enumerate_basins_row i [] = ([], i)
enumerate_basins_row i (False:bs) = (Nothing:cs, j)
    where (cs, j) = enumerate_basins_row i bs
enumerate_basins_row i (True:bs) = ((Just i):cs, j)
    where (cs, j) = enumerate_basins_row (i+1) bs

check :: [[Int]] -> [[Maybe Int]] -> Int -> Int -> Maybe Int
check map basins i j = if isJust ba then ba else (if sq == 9 then Nothing else check_square lower_neighbours)
    where sq = (map!!i)!!j
          ba = (basins!!i)!!j
          lower_neighbours = [b_n | (b_n, n) <- [up, down, left, right], n < sq]
          up = if i == 0 then (Nothing, 10) else ((basins!!(i-1))!!j, (map!!(i-1))!!j)
          down = if i == (length map - 1) then (Nothing, 10) else ((basins!!(i+1))!!j, (map!!(i+1))!!j)
          left = if j == 0 then (Nothing, 10) else ((basins!!i)!!(j-1), (map!!i)!!(j-1))
          right = if j == (length (head map) - 1) then (Nothing, 10) else ((basins!!i)!!(j+1), (map!!i)!!(j+1))

check_square :: [Maybe Int] -> Maybe Int
check_square ns
  | length ns == 0 = Nothing
  | otherwise      = if all (== head ns) ns then head ns else Nothing

flow_step :: [[Int]] -> [[Maybe Int]] -> [[Maybe Int]]
flow_step map basins = [[check map basins i j | j <- [0..length (head map) - 1]] | i <- [0..length map - 1]]

flow_n :: Int -> [[Int]] -> [[Maybe Int]] -> [[Maybe Int]]
flow_n 0 _ basins = basins
flow_n n map basins = flow_n (n-1) map (flow_step map basins)

count :: Eq a => a -> [[Maybe a]] -> Int
count i = sum . map (f i)
    where f i = length . filter (==Just i)

all_counts :: [[Maybe Int]] -> [Int]
all_counts basins = map (\i -> count i basins) [0..225]

counts_max :: [Int] -> Int
counts_max = foldl (*) 1 . take 3 . reverse . sort


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let heightmap = get_data strings
    let checked = check_lava heightmap
    let risk_map = risk heightmap checked
    let total_risk = (sum . map sum) risk_map
    let basins = enumerate_basins checked
    let basins_2 = flow_step heightmap basins
    let basins_n = flow_n 10 heightmap basins
    let counts = all_counts basins_n
    let final_result = counts_max counts
    putStrLn $ show total_risk
    putStrLn $ show basins
    putStrLn $ show basins_n
    putStrLn $ show counts
    putStrLn $ show final_result
