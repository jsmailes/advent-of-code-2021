import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split
import Data.Maybe (isJust, isNothing, fromJust)


-- PARSING --

type Segments = ([[Char]], [[Char]])
data Seg = A | B | C | D | E | F | G deriving (Eq, Ord, Show)

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


-- LOGIC PART 2 --

type Mapping = Seg -> Seg

to_seg :: Char -> Seg
to_seg 'a' = A
to_seg 'b' = B
to_seg 'c' = C
to_seg 'd' = D
to_seg 'e' = E
to_seg 'f' = F
to_seg 'g' = G

get_data_2 :: [([[Char]], [[Char]])] -> [([[Seg]], [[Seg]])]
get_data_2 = map f
    where f (css, dss) = (map (map to_seg) css, map (map to_seg) dss)

check :: [Seg] -> Maybe Int
check [A,B,C,E,F,G]   = Just 0
check [C,F]           = Just 1
check [A,C,D,E,G]     = Just 2
check [A,C,D,F,G]     = Just 3
check [B,C,D,F]       = Just 4
check [A,B,D,F,G]     = Just 5
check [A,B,D,E,F,G]   = Just 6
check [A,C,F]         = Just 7
check [A,B,C,D,E,F,G] = Just 8
check [A,B,C,D,F,G]   = Just 9
check _               = Nothing

apply_mapping :: Mapping -> [Seg] -> [Seg]
apply_mapping m = sort . map m

check_mapping :: Mapping -> [[Seg]] -> [[Seg]] -> Maybe Int
check_mapping m ls rs = if valid then Just (get_result (map fromJust rs')) else Nothing
    where ls' = map (check . apply_mapping m) ls
          rs' = map (check . apply_mapping m) rs
          valid = all isJust ls' && all isJust rs'

get_result :: [Int] -> Int
get_result (x:xs) = x * (10^(length xs)) + (get_result xs)
get_result [] = 0

all_mappings :: [Mapping]
all_mappings = [ f (p!!0) (p!!1) (p!!2) (p!!3) (p!!4) (p!!5) (p!!6) | p <- permutations [A,B,C,D,E,F,G] ]
    where f r _ _ _ _ _ _ A = r
          f _ r _ _ _ _ _ B = r
          f _ _ r _ _ _ _ C = r
          f _ _ _ r _ _ _ D = r
          f _ _ _ _ r _ _ E = r
          f _ _ _ _ _ r _ F = r
          f _ _ _ _ _ _ r G = r

check_mappings :: ([[Seg]], [[Seg]]) -> [Int]
check_mappings (ls, rs) = (map fromJust . filter isJust) [ check_mapping m ls rs | m <- all_mappings ]


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let segments = get_data strings
    let occurrences = count_1478 segments
    let segments' = get_data_2 segments
    let answers = map check_mappings segments'
    let final_answer = (sum . map head) answers
    putStrLn $ show segments
    putStrLn $ show occurrences
    putStrLn $ show segments'
    putStrLn $ show answers
    putStrLn $ show final_answer
