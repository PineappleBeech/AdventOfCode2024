import Data.Bits (xor)
import Data.List (iterate')

main = do
    contents <- lines <$> readFile "inputs/day22.txt"
    print $ sum $ map (calc iter . read) contents

prices :: Int -> [Int]
prices x = take 2000 $ map (`mod` 10) $ iterate' iter x

diff :: [Int] -> [Int]
diff (x:y:ys) = y - x : diff (y:ys)

value :: [Int] -> [Int] -> Int
value _ [_, _, _, _] = 0
value pattern (a:rest@(b:c:d:e:xs))
    | pattern == [b - a, c - b, d - c, e - d] = e
    | otherwise = value pattern rest

calc f x = iterate f x !! 2000

iter :: Int -> Int
iter = mp (* 2048) . mp (`div` 32) . mp (* 64)

mp f x = (x `xor` y) `mod` 16777216
    where y = f x