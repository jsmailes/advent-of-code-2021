import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List.Split

data Direction = Forward Int | Up Int | Down Int
data Position = Pos Int Int

move :: Direction -> Position -> Position
move (Forward x) (Pos h d) = Pos (h+x) d
move (Up x)      (Pos h d) = Pos h     (d-x)
move (Down x)    (Pos h d) = Pos h     (d+x)

final_pos :: [Direction] -> Position
final_pos moves = foldr move (Pos 0 0) moves

to_direction :: [Char] -> Int -> Direction
to_direction "forward" i = Forward i
to_direction "up"      i = Up i
to_direction "down"    i = Down i

to_data :: [Char] -> Direction
to_data cs = to_direction ((splitOn " " cs)!!0) (read ((splitOn " " cs)!!1))

distance :: Position -> Int
distance (Pos h d) = h * d

main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let directions = map to_data strings
    let pos = final_pos directions
    putStrLn $ show (distance pos)
