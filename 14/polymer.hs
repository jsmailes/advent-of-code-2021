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

type Polymer = [Char]
type Rule = (Char, Char, Char)

get_data :: [String] -> (Polymer, [Rule])
get_data lines = (head lines, map to_rule (drop 2 lines))

to_rule :: String -> Rule
to_rule cs = (cs!!0, cs!!1, cs!!6)


-- LOGIC --

advance_step :: [Rule] -> Polymer -> Polymer
advance_step _ [] = []
advance_step _ [x] = [x]
advance_step rs (x:y:xs) = if isJust r then x:(fromJust r):(advance_step rs (y:xs)) else x:(advance_step rs (y:xs))
    where r = check_rules rs x y

check_rules :: [Rule] -> Char -> Char -> Maybe Char
check_rules [] _ _ = Nothing
check_rules ((x, y, z):rs) x' y' = if (x == x') && (y == y') then Just z else check_rules rs x' y'

advance_steps :: Int -> [Rule] -> Polymer -> Polymer
advance_steps 0 _ p = p
advance_steps i rs p = advance_steps (i-1) rs p'
    where p' = advance_step rs p

polymer_counts :: Polymer -> [(Char, Int)]
polymer_counts = map swap . sort . map swap . frequency

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])


-- LOGIC PART 2 --

-- Okay so the idea here is we maintain a list of polymer pairs and their counts and just increment those each step

type PolyPair = ((Char, Char), Int)

get_pairs :: Polymer -> [PolyPair]
get_pairs = frequency . pairs
    where pairs (x:y:xs) = (x,y):(pairs (y:xs))
          pairs _ = []

find_count :: [PolyPair] -> Char -> Char -> Int
find_count ((p, c):ps) x y = if p == (x, y) then c else find_count ps x y
find_count [] _ _ = 0

apply_rules_polypairs :: [Rule] -> [PolyPair] -> [PolyPair]
apply_rules_polypairs [] _ = []
apply_rules_polypairs ((x, y, z):rs) ps = ((x, z), c):((z, y), c):(apply_rules_polypairs rs ps)
    where c = find_count ps x y

condense_pairs :: Eq a => [(a, Int)] -> [(a, Int)]
condense_pairs ((p, c):(p', c'):ps) = if p == p' then condense_pairs ((p, c+c'):ps) else (p, c):(condense_pairs ((p', c'):ps))
condense_pairs ps = ps

advance_step_polypairs :: [Rule] -> [PolyPair] -> [PolyPair]
advance_step_polypairs rs = condense_pairs . sort . apply_rules_polypairs rs

advance_steps_polypairs :: Int -> [Rule] -> [PolyPair] -> [PolyPair]
advance_steps_polypairs 0 _ ps = ps
advance_steps_polypairs i rs ps = advance_steps_polypairs (i-1) rs ps'
    where ps' = advance_step_polypairs rs ps

-- Needs to know what char went at the end since this method doesn't count it
get_counts :: [PolyPair] -> Char -> [(Char, Int)]
get_counts ps ch = (map swap . sort . map swap . condense_pairs . sort . ((ch,1):) . map f) ps
    where f ((u, v), c) = (u, c)

run_system_polypairs :: Int -> [Rule] -> Polymer -> [(Char, Int)]
run_system_polypairs i rs p = get_counts (advance_steps_polypairs i rs (get_pairs p)) (last p)


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let (poly, rules) = get_data strings
    let poly' = advance_steps 10 rules poly
    let poly_counts = polymer_counts poly'
    let answer_a = snd (last poly_counts) - snd (head poly_counts)
    let polypairs = get_pairs poly
    let poly_counts_b = run_system_polypairs 40 rules poly
    let answer_b = snd (last poly_counts_b) - snd (head poly_counts_b)
    putStrLn $ show poly
    putStrLn $ show rules
    putStrLn $ show poly_counts
    putStrLn $ show answer_a
    putStrLn $ show poly_counts_b
    putStrLn $ show answer_b
