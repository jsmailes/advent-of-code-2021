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

-- Literal: version, type ID, value
-- Operator: version, type ID, sub-packets
data Packet = Literal Int Int Int | Operator Int Int [Packet] deriving (Show)

get_data :: String -> [Int]
get_data = concatMap to_byte

to_byte :: Char -> [Int]
to_byte '0' = [0,0,0,0]
to_byte '1' = [0,0,0,1]
to_byte '2' = [0,0,1,0]
to_byte '3' = [0,0,1,1]
to_byte '4' = [0,1,0,0]
to_byte '5' = [0,1,0,1]
to_byte '6' = [0,1,1,0]
to_byte '7' = [0,1,1,1]
to_byte '8' = [1,0,0,0]
to_byte '9' = [1,0,0,1]
to_byte 'A' = [1,0,1,0]
to_byte 'B' = [1,0,1,1]
to_byte 'C' = [1,1,0,0]
to_byte 'D' = [1,1,0,1]
to_byte 'E' = [1,1,1,0]
to_byte 'F' = [1,1,1,1]


-- LOGIC --

to_int :: [Int] -> Int
to_int [] = 0
to_int (x:xs) = (x * 2^(length xs)) + (to_int xs)

parse :: [Int] -> (Packet, [Int])
parse bs
  | type_id == 4 = let parsed = parse_literal remaining in (Literal version type_id ((to_int . fst) parsed), snd parsed)
  | otherwise    = parse_operator bs
        where version = (to_int . take 3) bs
              type_id = (to_int . take 3 . drop 3) bs
              remaining = drop 6 bs

parse_operator :: [Int] -> (Packet, [Int])
parse_operator bs
  | length_type_id == 0 = let subpacket_length = (to_int . take 15 . drop 7) bs in (Operator version type_id (parse_subpackets ((take subpacket_length . drop 22) bs)), drop (22+subpacket_length) bs)
  | otherwise           = let subpacket_count = (to_int . take 11 . drop 7) bs in let (ps, remaining) = parse_num_subpackets subpacket_count (drop 18 bs) in (Operator version type_id ps, remaining)
        where version = (to_int . take 3) bs
              type_id = (to_int . take 3 . drop 3) bs
              length_type_id = (to_int . take 1 . drop 6) bs

parse_subpackets :: [Int] -> [Packet]
parse_subpackets bs
  | length bs < 8 = []
  | otherwise = let (packet, remaining) = parse bs in packet:(parse_subpackets remaining)

parse_num_subpackets :: Int -> [Int] -> ([Packet], [Int])
parse_num_subpackets 0 bs = ([], bs)
parse_num_subpackets i bs = (p:ps, remaining')
    where (p, remaining) = parse bs
          (ps, remaining') = parse_num_subpackets (i-1) remaining

parse_literal :: [Int] -> ([Int], [Int])
parse_literal (b:bs)
  | b == 0    = (take 4 bs, drop 4 bs)
  | otherwise = let parsed = (parse_literal . drop 4) bs in ((take 4 bs) ++ (fst parsed), snd parsed)

version_sum :: Packet -> Int
version_sum (Literal v _ _) = v
version_sum (Operator v _ ps) = ((v+) . sum . map version_sum) ps

packet_value :: Packet -> Int
packet_value (Literal _ _ v) = v
packet_value (Operator _ t ps)
  | t == 0    = sum vs
  | t == 1    = product vs
  | t == 2    = minimum vs
  | t == 3    = maximum vs
  | t == 5    = if (vs!!0) > (vs!!1) then 1 else 0
  | t == 6    = if (vs!!0) < (vs!!1) then 1 else 0
  | t == 7    = if (vs!!0) == (vs!!1) then 1 else 0
  | otherwise = error "Invalid operator type ID"
        where vs = map packet_value ps


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let bits = get_data (head strings)
    let (packet, remaining) = parse bits
    let v_sum = version_sum packet
    let final_value = packet_value packet
    putStrLn $ show bits
    putStrLn $ show packet
    putStrLn $ show v_sum
    putStrLn $ show final_value
