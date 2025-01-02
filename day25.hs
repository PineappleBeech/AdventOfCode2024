import Data.List (transpose)
import Debug.Trace (traceShowId)
main = do
    contents <- readFile "inputs/day25.txt"
    let l = process contents
    print $ length $ filter check $ combos l

process s = if length s >= 42 then f (take 43 s) : process (drop 43 s) else []
    where f s = (map (length . filter (== '#')) $ transpose $ lines s, head s == '.')

combos :: [(a, Bool)] -> [(a, a)]
combos [] = []
combos ((l, key):xs) = map ((l,) . fst) (filter ((/= key) . snd) xs) ++ combos xs

check (a, b) = all ((<= 7) . uncurry (+)) $ zip a b