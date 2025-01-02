main = do
    contents <-  readFile "inputs/day2.txt"
    let report :: [[Int]] = map (map read) $ map words $ lines contents
    print (count increasing report + count decreasing report)

count :: ([Int] -> Bool) -> [[Int]] -> Int
count _ [] = 0
count f (x:xs)
    | f x = 1 + count f xs
    | otherwise = count f xs

increasing :: [Int] -> Bool
increasing [] = True
increasing [_] = True
increasing (x:y:ys)
    | y <= x = False
    | y > x + 3 = False
    | otherwise = increasing (y:ys)

decreasing :: [Int] -> Bool
decreasing [] = True
decreasing [_] = True
decreasing (x:y:ys)
    | y >= x = False
    | y < x - 3 = False
    | otherwise = decreasing (y:ys)