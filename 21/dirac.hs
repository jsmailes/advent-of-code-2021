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

-- Head element yields next value to be rolled
type Die = [Int]
-- (Position, Score)
type Player = (Int, Int)
-- Current die state, number of die rolls, state of each player
type Game = (Die, Int, Player, Player)

init_state :: Game
init_state = (deterministic_die 100, 0, init_player1, init_player2)
    where init_player1 = (8, 0)
          init_player2 = (2, 0)

deterministic_die :: Int -> Die
deterministic_die num_sides = map ((+1) . (flip mod num_sides)) [0..]


-- LOGIC --

player_done :: Player -> Int -> Bool
player_done (_, s) sc = s >= sc

final_score :: Game -> Maybe Int
final_score (_, n, p1, p2)
  | player_done p1 1000 = Just $ snd p2 * n
  | player_done p2 1000 = Just $ snd p1 * n
  | otherwise           = Nothing

update_player :: Int -> Game -> Game
update_player 1 (d, n, (pos, score), p2) = (drop 3 d, n+3, (pos', score'), p2)
    where rolls = sum $ take 3 d
          pos' = (mod (pos - 1 + rolls) 10) + 1
          score' = score + pos'
update_player 2 (d, n, p1, (pos, score)) = (drop 3 d, n+3, p1, (pos', score'))
    where rolls = sum $ take 3 d
          pos' = (mod (pos - 1 + rolls) 10) + 1
          score' = score + pos'

run_game :: Game -> [Game]
run_game g = g:g':(run_game g'')
    where g' = update_player 1 g
          g'' = update_player 2 g'

-- One potential solution is to get a transition matrix, we don't care about die rolls so we only have 10*10*21*21 possible states
-- Figure out a transition matrix or transition function, define that and lazily evaluate, then compute counts backwards?

nondeterministic_turn :: Int -> [Die] -> Game -> [Game]
nondeterministic_turn p ds (_, _, p1, p2) = map (\d -> update_player p (d, 0, p1, p2)) ds

nondeterministic_turns :: Int -> [Die] -> [(Game, Int)] -> [(Game, Int)]
nondeterministic_turns p ds = condense . sort . concatMap (\(g, i) -> map (\g' -> (g', i)) (nondeterministic_turn p ds g))
    where condense [] = []
          condense [x] = [x]
          condense ((g, i):(g', i'):xs) = if g == g' then condense ((g, i+i'):xs) else (g, i):(condense ((g', i'):xs))

extract :: Int -> [(Game, Int)] -> ([(Game, Int)], Int)
extract p gs = (gs', s)
    where gs' = filter (not . flip player_done 21 . get_player p . fst) gs
          s = (sum . map snd . filter (flip player_done 21 . get_player p . fst)) gs
          get_player 1 (_, _, p1, _) = p1
          get_player 2 (_, _, _, p2) = p2

nondeterministic_die :: Int -> [Die]
nondeterministic_die n = [ [a,b,c] | a <- [1..n], b <- [1..n], c <- [1..n] ]

run_turn :: [Die] -> [(Game, Int)] -> ([(Game, Int)], Int, Int)
run_turn ds gs = (gs'', s1, s2)
    where (gs', s1)  = extract 1 $ nondeterministic_turns 1 ds gs
          (gs'', s2) = extract 2 $ nondeterministic_turns 2 ds gs'

run_turns :: [Die] -> [(Game, Int)] -> (Int, Int)
run_turns ds gs = if length gs' == 0 then (s1, s2) else (s1+s1', s2+s2')
    where (gs', s1, s2) = run_turn ds gs
          (s1', s2')    = run_turns ds gs'

run_nondeterministic_game :: [Die] -> Game -> (Int, Int)
run_nondeterministic_game ds g = run_turns ds [(g, 1)]


main = do
    let state = init_state
    let states = run_game state
    let final_state = head . dropWhile isNothing . map final_score $ states
    let counts = run_nondeterministic_game (nondeterministic_die 3) state
    let winner = uncurry max counts
    putStrLn $ show final_state
    putStrLn $ show counts
    putStrLn $ show winner
