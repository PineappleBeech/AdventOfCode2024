import Data.List (sort)
main = do
    contents <- readFile "inputs/day1.txt"
    print $ sum $ map diff $ uncurry zip $ dualsort $ unzip $ map splitline $ lines contents


splitline :: String -> (Int, Int)
splitline [a1, a2, a3, a4, a5, _, _, _, b1, b2, b3, b4, b5] = (read [a1, a2, a3, a4, a5], read [b1, b2, b3, b4, b5])

diff :: (Int, Int) -> Int
diff (x, y) = abs (x - y)

dualsort :: ([Int], [Int]) -> ([Int], [Int])
dualsort (x, y) = (sort x, sort y)
