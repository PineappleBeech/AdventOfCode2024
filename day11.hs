main = do
    contents <- readFile "inputs/day11.txt"
    let start = map read $ words contents
    print $ length $ iterate iter start !! 25

iter :: [Integer] -> [Integer]
iter [] = []
iter (0:xs) = 1 : iter xs
iter (x:xs)
    | even (length s) = let (a, b) = splitAt (length s `div` 2) s
                        in read a : read b : iter xs
    | otherwise = x * 2024 : iter xs
    where s = show x