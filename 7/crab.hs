import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text
import Data.List
import Data.List.Split


-- PARSING --

get_data :: [[Char]] -> [Int]
get_data lines = (map read . splitOn "," . head) lines


-- LOGIC --

diffs :: Int -> [Int] -> Int
diffs i = sum . map (abs . (i-))

diffs_tri :: Int -> [Int] -> Int
diffs_tri i = sum . map (tri . abs . (i-))

tri :: Int -> Int
tri n = tri_tr n 0

tri_tr :: Int -> Int -> Int
tri_tr 0 x = x
tri_tr n x = tri_tr (n-1) (x+n)


main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let strings = map Text.fromText ls
    let crabs = get_data strings
    let dists = map (\i -> diffs i crabs) [0..maximum crabs]
    let min_dist = minimum dists
    let dists_tri = map (\i -> diffs_tri i crabs) [0..maximum crabs]
    let min_dist_tri = minimum dists_tri
    --let min_dist = snd (minimum (zip dists [0..]))
    putStrLn $ show crabs
    putStrLn $ show min_dist
    putStrLn $ show min_dist_tri
