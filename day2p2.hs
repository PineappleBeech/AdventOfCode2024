main = do
    contents <-  readFile "inputs/day2.txt"
    let report :: [[Int]] = map (map read) $ map words $ lines contents
    --print (count (\x -> increasingShield x || decreasingShield x) report)
    print (count (any safe . skip) report)

count :: ([Int] -> Bool) -> [[Int]] -> Int
count _ [] = 0
count f (x:xs)
    | f x = 1 + count f xs
    | otherwise = count f xs

increasingShield :: [Int] -> Bool
increasingShield [] = True
increasingShield [_] = True
increasingShield (x:y:ys)
    | y <= x = increasing (y:ys)
    | y > x + 3 = increasing (y:ys)
    | otherwise = increasingShield (y:ys)

increasing :: [Int] -> Bool
increasing [] = True
increasing [_] = True
increasing (x:y:ys)
    | y <= x = False
    | y > x + 3 = False
    | otherwise = increasing (y:ys)

decreasingShield :: [Int] -> Bool
decreasingShield [] = True
decreasingShield [_] = True
decreasingShield (x:y:ys)
    | y >= x = decreasing (y:ys)
    | y < x - 3 = decreasing (y:ys)
    | otherwise = decreasingShield (y:ys)

decreasing :: [Int] -> Bool
decreasing [] = True
decreasing [_] = True
decreasing (x:y:ys)
    | y >= x = False
    | y < x - 3 = False
    | otherwise = decreasing (y:ys)

skip :: [Int] -> [[Int]]
skip [] = []
skip (x:xs) = xs : map (x:) (skip xs)

safe :: [Int] -> Bool
safe x = increasing x || decreasing x