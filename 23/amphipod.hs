import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Map (fromListWith, toList)
import Data.Tuple (swap)


-- LOGIC --

data Cell = A | B | C | D deriving (Eq)
type Map = [Maybe Cell]

-- ######################################
-- ## 00 01 02 03 04 05 06 07 08 09 10 ##
-- ######## 11 ## 13 ## 15 ## 17 ########
-- ######## 12 ## 14 ## 16 ## 18 ########
-- ######################################

adjacent :: Int -> Int -> Bool
adjacent x y = adj x y || adj y x
    where adj 00 01 = True
          adj 01 02 = True
          adj 02 03 = True
          adj 03 04 = True
          adj 04 05 = True
          adj 05 06 = True
          adj 06 07 = True
          adj 07 08 = True
          adj 08 09 = True
          adj 09 10 = True
          adj 02 11 = True
          adj 11 12 = True
          adj 04 13 = True
          adj 13 14 = True
          adj 06 15 = True
          adj 15 16 = True
          adj 08 17 = True
          adj 17 18 = True
          adj _  _  = False

neighbours :: Int -> [Int]
neighbours 00 = [01]
neighbours 01 = [02, 00]
neighbours 02 = [01, 03, 11]
neighbours 03 = [02, 04]
neighbours 04 = [03, 05, 13]
neighbours 05 = [04, 06]
neighbours 06 = [05, 07, 15]
neighbours 07 = [06, 08]
neighbours 08 = [07, 09, 17]
neighbours 09 = [08, 10]
neighbours 10 = [09]
neighbours 11 = [02, 12]
neighbours 12 = [11]
neighbours 13 = [04, 14]
neighbours 14 = [13]
neighbours 15 = [06, 16]
neighbours 16 = [15]
neighbours 17 = [08, 18]
neighbours 18 = [17]
neighbours _  = []

can_stop :: Int -> Bool
can_stop _ = False

occupied :: Int -> Map -> Bool
occupied i = isJust . (!!i)

empty :: Int -> Map -> Bool
empty i = isNothing . (!!i)

energy :: Cell -> Int
energy A = 1
energy B = 10
energy C = 100
energy D = 1000

edges :: Map -> (Map, Int)

done :: Map -> Bool
done map = drop 11 map == [Just A, Just A, Just B, Just B, Just C, Just C, Just D, Just D]


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = 0
    putStrLn $ show 0
