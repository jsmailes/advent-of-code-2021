import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split


-- PARSING --

type Segments = ([[Char]], [[Char]])

get_data :: [[Char]] -> [([[Char]], [[Char]])]
get_data = map (f . splitOn ["|"] . splitOn " ")
    where f css = (css!!0, css!!1)


-- LOGIC --

all_lengths :: [Segments] -> [[Int]]
all_lengths = map (map length . snd)

count_occurrences :: Eq a => a -> [a] -> Int
count_occurrences x = length . filter (==x)

count_1478 :: [Segments] -> Int
count_1478 segments = count_1s + count_4s + count_7s + count_8s
    where count_1s = count_occurrences 2 lengths
          count_4s = count_occurrences 4 lengths
          count_7s = count_occurrences 3 lengths
          count_8s = count_occurrences 7 lengths
          lengths = concat (all_lengths segments)


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let segments = get_data strings
    let occurrences = count_1478 segments
    putStrLn $ show segments
    putStrLn $ show occurrences
