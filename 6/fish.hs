import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split


-- PARSING --

get_data :: [[Char]] -> [Int]
get_data lines = (map read . splitOn "," . head) lines


-- LOGIC --

advance_day :: [Int] -> [Int]
advance_day [] = []
advance_day (0:xs) = 6:8:(advance_day xs)
advance_day (x:xs) = (x-1):(advance_day xs)

advance_days :: [Int] -> Int -> [Int]
advance_days xs 0 = xs
advance_days xs i = advance_days (advance_day xs) (i-1)

advance_day_newlogic :: [Int] -> [Int]
advance_day_newlogic (x0:x1:x2:x3:x4:x5:x6:x7:x8:_) = x1:x2:x3:x4:x5:x6:(x7+x0):x8:x0:[]

advance_days_newlogic :: [Int] -> Int -> [Int]
advance_days_newlogic xs 0 = xs
advance_days_newlogic xs i = advance_days_newlogic (advance_day_newlogic xs) (i-1)

count :: [Int] -> Int -> Int
count xs i = (length . filter (==i)) xs

to_newlogic :: [Int] -> [Int]
to_newlogic xs = [count xs i | i <- [0..8]]


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let fish = get_data strings
    let fish_mid = advance_days fish 80
    let fish_newlogic = to_newlogic fish
    let fish_final = advance_days_newlogic fish_newlogic 256
    putStrLn $ show fish
    putStrLn $ show (length fish_mid)
    putStrLn $ show (sum fish_final)
