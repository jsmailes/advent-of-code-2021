import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List.Split

to_bits :: [Char] -> [Int]
to_bits = map (read . (:[]))

-- Ok so we have a list of [1,0,1,1,0] or whatever
-- We want to be able to sum each of the lists elementwise
totals :: [[Int]] -> [Int]
totals elems = foldl (zipWith (+)) (repeat 0) elems

most_common :: [Int] -> Int -> [Int]
most_common ts len = map (\x -> if x >= (div len 2 + rem len 2) then 1 else 0) ts

least_common :: [Int] -> Int -> [Int]
least_common ts len = map (\x -> if x < (div len 2 + rem len 2) then 1 else 0) ts

invert :: [Int] -> [Int]
invert = map (1-)

binary_to_int :: [Int] -> Int
binary_to_int xs = foldl (\y x -> y*2 + x) 0 xs

aaa :: [[Int]]
aaa = [[0,0,1,0,0],[1,1,1,1,0],[1,0,1,1,0],[1,0,1,1,1],[1,0,1,0,1],[0,1,1,1,1],[0,0,1,1,1],[1,1,1,0,0],[1,0,0,0,0],[1,1,0,0,1],[0,0,0,1,0],[0,1,0,1,0]]

filter_step_ox :: [[Int]] -> Int -> [[Int]]
filter_step_ox xss i = filter ((==m) . (!!i)) xss
    where m = (most_common (totals xss) (length xss))!!i

filter_ox :: [[Int]] -> Int -> [Int]
filter_ox (xs:[]) _ = xs
filter_ox xss i = filter_ox (filter_step_ox xss i) (i+1)

filter_step_co :: [[Int]] -> Int -> [[Int]]
filter_step_co xss i = filter ((==m) . (!!i)) xss
    where m = (least_common (totals xss) (length xss))!!i

filter_co :: [[Int]] -> Int -> [Int]
filter_co (xs:[]) _ = xs
filter_co xss i = filter_co (filter_step_co xss i) (i+1)

--filter_step_ox :: [([Int], Int)] -> [([Int], Int)]
--filter_step_ox elems = map (\(a, b) -> (tail a, b)) (filter ((==m) . head . fst) elems)
--    where m = head (most_common (totals xss) (length xss))
--          xss = map fst elems

--filter_ox :: [([Int], Int)] -> ([Int], Int)
--filter_ox (xs:[]) = xs
--filter_ox xss = filter_ox (filter_step_ox xss)

--filter_step_co :: [[Int]] -> [[Int]]
--filter_step_co xss = map tail (filter ((==m) . head) xss)
--    where m = head (least_common (totals xss) (length xss))

--filter_co :: [[Int]] -> [Int]
--filter_co (xs:[]) = xs
--filter_co xss = filter_co (filter_step_co xss)


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let bits = map to_bits strings
    let ts = totals bits
    let gamma = most_common ts (length bits)
    let epsilon = invert gamma
    let ox = binary_to_int (filter_ox bits 0)
    let co = binary_to_int (filter_co bits 0)
    putStrLn $ show gamma
    putStrLn $ show (binary_to_int gamma)
    putStrLn $ show epsilon
    putStrLn $ show (binary_to_int epsilon)
    putStrLn $ show ((binary_to_int gamma) * (binary_to_int epsilon))
    putStrLn $ show ox
    putStrLn $ show co
    putStrLn $ show (ox * co)
