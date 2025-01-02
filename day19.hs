import Control.Monad (guard)
import Data.Text (unpack, pack, splitOn)

main = do
    contents <- readFile "inputs/day19.txt"
    let l = lines contents
        patterns :: [String] = map unpack $ splitOn (pack ", ") $ pack $ head l
        strings = drop 2 l

    print $ length $ filter (check patterns) strings


check patterns = not . null . find patterns

find :: [String] -> String -> [[String]]
find patterns s = do
    p <- patterns
    guard (checkPartial p s)
    if length p == length s 
        then return [p]
        else  map (p :) (find patterns (drop (length p) s))

checkPartial p s = and (zipWith (==) p s) && length p <= length s