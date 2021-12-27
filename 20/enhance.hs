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

type Algorithm = [Int]
type Image = [[Int]]

get_data :: [String] -> (Algorithm, Image)
get_data lines = ((get_algorithm . head) lines, (get_image . drop 2) lines)

get_algorithm :: String -> Algorithm
get_algorithm = map from_lightdark

get_image :: [String] -> Image
get_image = map (map from_lightdark)

from_lightdark :: Char -> Int
from_lightdark '#' = 1
from_lightdark '.' = 0
from_lightdark _   = error "Unknown character"

-- LOGIC --

sum_row :: Int -> [[Int]] -> [[Int]]
sum_row a r = zipWith (++) (zipWith (++) r' (drop 1 r')) (drop 2 r')
    where r' = ([a]):([a]):(r ++ [[a],[a]])

sum_column :: Int -> [[Int]] -> [[Int]]
sum_column a r = zipWith (++) (zipWith (++) r' (drop 1 r')) (drop 2 r')
    where r' = ([a,a,a]):([a,a,a]):(r ++ [[a,a,a],[a,a,a]])

sum_image :: Int -> Image -> [[[Int]]]
sum_image a = transpose . map (sum_column a) . transpose . map (sum_row a) . map (map (:[]))

process_image :: Int -> Algorithm -> Image -> Image
process_image a alg = map (map (apply_algorithm alg)) . sum_image a

get_images :: Algorithm -> Image -> [Image]
get_images alg image = image:image':(get_images alg image'')
    where image' = process_image 0 alg image
          image'' = process_image a alg image'
          a = head alg

apply_algorithm :: Algorithm -> [Int] -> Int
apply_algorithm alg bs = alg!!(to_int bs)

to_int :: [Int] -> Int
to_int []     = 0
to_int (x:xs) = (x * 2^(length xs)) + (to_int xs)

test_alg :: Algorithm
test_alg = get_algorithm "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"

test_image :: Image
test_image = [[1,0,0,1,0],[1,0,0,0,0],[1,1,0,0,1],[0,0,1,0,0],[0,0,1,1,1]]


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let (alg, image) = get_data strings
    let images = get_images alg image
    let image' = images!!2
    let count = sum $ map sum image'
    let image'' = images!!50
    let count' = sum $ map sum image''
    putStrLn $ show alg
    putStrLn $ show count
    putStrLn $ show count'
