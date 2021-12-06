import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Conversions as Text

depths :: [Int] -> Int
depths xs = (sum . map (fromEnum . uncurry (<))) (zip xs (tail xs))

windows :: [Int] -> [Int]
windows xs = zipWith (+) (zipWith (+) xs (tail xs)) ((tail . tail) xs)

main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let xs = map (read . Text.fromText) ls
    let d = depths xs
    putStrLn $ show d
    let dd = (depths . windows) xs
    putStrLn $ show dd
