import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List.Split

data Direction = Forward Integer | Up Integer | Down Integer deriving (Show)
data Position = Pos Integer Integer Integer deriving (Show)
-- Pos horizontal_pos depth aim

move :: Position -> Direction -> Position
move (Pos h d a) (Forward x) = Pos (h+x) (d+(a*x)) a
move (Pos h d a) (Up x)      = Pos h d (a-x)
move (Pos h d a) (Down x)    = Pos h d (a+x)

final_pos :: [Direction] -> Position
final_pos moves = foldl move (Pos 0 0 0) moves

to_direction :: [Char] -> Integer -> Direction
to_direction "forward" i = Forward i
to_direction "up"      i = Up i
to_direction "down"    i = Down i

to_data :: [Char] -> Direction
to_data cs = to_direction ((splitOn " " cs)!!0) (read ((splitOn " " cs)!!1))

distance :: Position -> Integer
distance (Pos h d a) = h * d

main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let directions = map to_data strings
    let pos = final_pos directions
    putStrLn $ show (distance pos)
