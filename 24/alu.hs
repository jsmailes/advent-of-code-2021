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

data Register = W | X | Y | Z deriving (Eq, Show)
data Arg = Reg Register | Imm Int deriving (Eq, Show)
data Instruction = Inp Register | Add Register Arg | Mul Register Arg | Div Register Arg | Mod Register Arg | Eql Register Arg deriving (Eq, Show)
type State = (Int, Int, Int, Int, [Int])

get_data :: [String] -> [Instruction]
get_data = map parse_instruction

parse_instruction :: String -> Instruction
parse_instruction s
  | instr == "inp" = Inp arg0
  | instr == "add" = Add arg0 arg1
  | instr == "mul" = Mul arg0 arg1
  | instr == "div" = Div arg0 arg1
  | instr == "mod" = Mod arg0 arg1
  | instr == "eql" = Eql arg0 arg1
  | otherwise      = error "Unknown instruction"
    where instr = take 3 s
          arg0 = fromJust $ parse_reg (s!!4)
          arg1 = parse_arg (drop 6 s)

parse_reg :: Char -> Maybe Register
parse_reg 'w' = Just W
parse_reg 'x' = Just X
parse_reg 'y' = Just Y
parse_reg 'z' = Just Z
parse_reg _   = Nothing

parse_arg :: String -> Arg
parse_arg s
  | (isJust . parse_reg . head) s = (Reg . fromJust . parse_reg . head) s
  | otherwise                     = (Imm . read) s


-- LOGIC --

init_state :: [Int] -> State
init_state inputs = (0, 0, 0, 0, inputs)

get_register :: Register -> State -> Int
get_register W (w, _, _, _, _) = w
get_register X (_, x, _, _, _) = x
get_register Y (_, _, y, _, _) = y
get_register Z (_, _, _, z, _) = z

get_arg :: Arg -> State -> Int
get_arg (Imm i) _ = i
get_arg (Reg r) s = get_register r s

set_register :: Register -> State -> Int -> State
set_register W (w, x, y, z, i) w' = (w', x,  y,  z,  i)
set_register X (w, x, y, z, i) x' = (w,  x', y,  z,  i)
set_register Y (w, x, y, z, i) y' = (w,  x,  y', z,  i)
set_register Z (w, x, y, z, i) z' = (w,  x,  y,  z', i)

process_instruction :: State -> Instruction -> State
process_instruction (w, x, y, z, inputs) (Inp r)   = let (w', x', y', z', _) = set_register r (w, x, y, z, inputs) (head inputs) in (w', x', y', z', tail inputs)
process_instruction s (Add a b) = set_register a s (get_register a s + get_arg b s)
process_instruction s (Mul a b) = set_register a s (get_register a s * get_arg b s)
process_instruction s (Div a b) = set_register a s (get_register a s `div` get_arg b s)
process_instruction s (Mod a b) = set_register a s (get_register a s `mod` get_arg b s)
process_instruction s (Eql a b) = set_register a s (if get_register a s == get_arg b s then 1 else 0)

run_program :: State -> [Instruction] -> State
run_program = foldl process_instruction

check_model :: [Instruction] -> [Int] -> Bool
check_model instrs xs = let (_, _, _, z, _) = run_program (init_state xs) instrs in z == 0

inputs :: Int -> [[Int]]
inputs 0 = []
inputs 1 = [ [a] | a <- [9,8..1] ]
inputs i = [ a:xs | a <- [9,8..1], xs <- inputs (i-1) ]

valids :: [Instruction] -> [[Int]]
valids instrs = [ xs | xs <- inputs 14, check_model instrs xs ]


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let instructions = get_data strings
    let result = check_model instructions [1,3,5,7,9,2,4,6,8,9,9,9,9,9]
    let vs = valids instructions
    putStrLn $ show instructions
    putStrLn $ show result
    putStrLn $ show (head vs)
