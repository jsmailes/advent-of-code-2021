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

type Beacon = (Int, Int, Int)

get_data :: [String] -> [[Beacon]]
get_data lines = []


-- LOGIC --

-- So we need to check each pair of scanners to see if they overlap
-- Choose one beacon from each scanner, convert relative to 0 for that beacon
-- Choose another beacon from each scnaner, see if they match up for any rotation
-- If so, check how many others match up

sub :: Beacon -> Beacon -> Beacon
sub (b0, b1, b2) (c0, c1, c2) = (b0-c0, b1-c1, b2-c2)

convert_relative :: Beacon -> [Beacon] -> [Beacon]
convert_relative b bs = map (\b' -> sub b' b) bs

rotate :: [Beacon] -> [Beacon]
rotate = map rotate_beacon

rotate_beacon :: Beacon -> Beacon
rotate_beacon 


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let scanners = get_data strings
    putStrLn $ show scanners
