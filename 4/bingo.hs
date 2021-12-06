import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split


-- PARSING --

get_data :: [[Char]] -> ([Int], [[[Int]]])
get_data lines = (get_draws (head lines), get_bingos (tail (tail lines)))

get_draws :: [Char] -> [Int]
get_draws cs = (map read . splitOn ",") cs

get_bingos :: [[Char]] -> [[[Int]]]
get_bingos [] = []
get_bingos css = (get_bingo (take 5 css)):(get_bingos (drop 6 css))

get_bingo :: [[Char]] -> [[Int]]
get_bingo [] = []
get_bingo (cs:css) = ((map read . words) cs):(get_bingo css)

get :: Int -> a -> [a]
get n x = (take n . repeat) x

init_state :: Int -> [[[Bool]]]
init_state n = get n (get 5 (get 5 False))


-- LOGIC --

-- Check if bss is currently a winning board
is_winning :: [[Bool]] -> Bool
is_winning bss = (any (all id) bss) || (any (all id) (transpose bss))

-- Get the final score for board (xss,bss) with final number s
score :: [[Int]] -> [[Bool]] -> Int -> Int
score xss bss s = s * ((sum . map sum . zipWith f xss) css)
    where css = map (map (fromEnum . not)) bss
          f xs cs = zipWith (*) xs cs

-- Mark a given square
mark :: [[Int]] -> [[Bool]] -> Int -> [[Bool]]
mark [] [] _ = []
mark (xs:xss) (bs:bss) s = (f xs bs s):(mark xss bss s)
    where f :: [Int] -> [Bool] -> Int -> [Bool]
          f [] [] s = []
          f (x:xs) (b:bs) s = (if x == s then True else b):(f xs bs s)

-- Advance the state by one
advance :: [[[Int]]] -> [[[Bool]]] -> Int -> [[[Bool]]]
advance bingos states s = zipWith (\bingo state -> mark bingo state s) bingos states

-- Returns Some score if we're done, otherwise Nothing
is_done :: [[[Int]]] -> [[[Bool]]] -> Int -> Maybe Int
is_done bingos states s = check (zipWith f winning scores)
    where winning = map is_winning states
          scores = zipWith (\bingo state -> score bingo state s) bingos states
          f w sc = if w then Just sc else Nothing
          check [] = Nothing
          check (Nothing:xs) = check xs
          check (Just x:xs) = Just x

compute :: [[[Int]]] -> [[[Bool]]] -> [Int] -> Int
compute _ _ [] = 0
compute bingos states (x:xs) = f bingos states_new (is_done bingos states_new x) x xs
    where states_new = advance bingos states x
          f bingos new_states Nothing x xs = compute bingos new_states xs
          f bingos new_states (Just a) x xs = a

-- Returns the index of the board that wins last if it's the only one left
is_done_losing :: [[[Int]]] -> [[[Bool]]] -> Int -> Maybe Int
is_done_losing bingos states s = check (zipWith f winning [0..])
    where winning = map is_winning states
          f w sc = if w then (True, sc) else (False, sc)
          check xs = if count_false xs == 1 then false_score xs else Nothing
          count_false [] = 0
          count_false ((False, _):xs) = 1 + (count_false xs)
          count_false ((True, _):xs) = count_false xs
          false_score [] = Nothing
          false_score ((False, x):xs) = Just x
          false_score ((True, _):xs) = false_score xs

-- Return the index of the board that wins last
compute_losing :: [[[Int]]] -> [[[Bool]]] -> [Int] -> Int
compute_losing _ _ [] = 0
compute_losing bingos states (x:xs) = f bingos states_new (is_done_losing bingos states_new x) x xs
    where states_new = advance bingos states x
          f bingos new_states Nothing x xs = compute_losing bingos new_states xs
          f bingos new_states (Just a) x xs = a

-- Advance board until it's done and return the score
finish_board :: [[Int]] -> [[Bool]] -> [Int] -> Int
finish_board bingo state (x:xs) = f bingo state_new (is_winning state_new) x xs
    where state_new = mark bingo state x
          f bingo state_new am_winning x xs = if am_winning then score bingo state_new x else finish_board bingo state_new xs


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let (lines, bingos) = get_data strings
    let init = init_state (length bingos)
    let result = compute bingos init lines
    let losing_board = compute_losing bingos init lines
    let result_losing = finish_board (bingos!!losing_board) (init!!losing_board) lines
    putStrLn $ show lines
    putStrLn $ show bingos
    putStrLn $ show result
    putStrLn $ show losing_board
    putStrLn $ show result_losing
