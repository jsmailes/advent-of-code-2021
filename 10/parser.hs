import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split
import Data.Maybe


-- LOGIC --

data Status = Good | Incomplete [Char] | Corrupted Char deriving (Show)

opening :: Char -> Bool
opening '(' = True
opening '[' = True
opening '{' = True
opening '<' = True
opening _   = False

check :: Char -> Char -> Bool
check '(' ')' = True
check '[' ']' = True
check '{' '}' = True
check '<' '>' = True
check _   _   = False

-- First list is line to be consumed, second list is stack
parse_line :: [Char] -> [Char] -> Status
parse_line [] [] = Good
parse_line [] ds = Incomplete ds
parse_line (c:cs) ds
  | opening c         = parse_line cs (c:ds)
  | length ds == 0    = Corrupted 'e'
  | check (head ds) c = parse_line cs (tail ds)
  | otherwise         = Corrupted c

parse_line_test :: [Char] -> [Char] -> [Char]
parse_line_test [] [] = []
parse_line_test [] _ = []
parse_line_test (c:cs) ds
  | opening c         = parse_line_test cs (c:ds)
  | length ds == 0    = ['e']
  | check (head ds) c = parse_line_test cs (tail ds)
  | otherwise         = ds

parse_lines :: [[Char]] -> [Status]
parse_lines ls = map (\l -> parse_line l []) ls

score :: Status -> Int
score (Corrupted ')') = 3
score (Corrupted ']') = 57
score (Corrupted '}') = 1197
score (Corrupted '>') = 25137
score (Incomplete _)  = 0
score Good            = 0

total_score :: [Status] -> Int
total_score = sum . map score

completion_string :: [Char] -> [Char]
completion_string [] = []
completion_string ('(':xs) = ')':(completion_string xs)
completion_string ('[':xs) = ']':(completion_string xs)
completion_string ('{':xs) = '}':(completion_string xs)
completion_string ('<':xs) = '>':(completion_string xs)

completion_score :: [Char] -> Int
completion_score = partial_score 0
    where partial_score i [] = i
          partial_score i (x:xs) = partial_score (5*i + charscore x) xs

charscore :: Char -> Int
charscore ')' = 1
charscore ']' = 2
charscore '}' = 3
charscore '>' = 4
charscore _   = error "Unexpected character"

completion_scores :: [Status] -> [Int]
completion_scores = map f
    where f (Incomplete cs) = completion_score (completion_string cs)
          f _ = 0

middle_score :: [Int] -> Int
middle_score xs = ys!!i
    where ys = (sort . filter (/= 0)) xs
          i  = div (length ys) 2


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let statuses = parse_lines strings
    let total = total_score statuses
    let completion = completion_scores statuses
    let middle = middle_score completion
    putStrLn $ show statuses
    putStrLn $ show total
    putStrLn $ show completion
    putStrLn $ show middle
