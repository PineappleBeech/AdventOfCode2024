import Data.List (sort)
import Data.IntMap.Strict (IntMap, empty, alter, findWithDefault)
main = do
    contents <- readFile "inputs/day1.txt"
    let lists = dualsort $ unzip $ map splitline $ lines contents
    let m = genMap $ snd lists
    print $ count m $ fst lists


splitline :: String -> (Int, Int)
splitline [a1, a2, a3, a4, a5, _, _, _, b1, b2, b3, b4, b5] = (read [a1, a2, a3, a4, a5], read [b1, b2, b3, b4, b5])

splitline' :: String -> (Int, Int)
splitline' [a1, _, _, _, b1] = (read [a1], read [b1])


dualsort :: ([Int], [Int]) -> ([Int], [Int])
dualsort (x, y) = (sort x, sort y)

genMap :: [Int] -> IntMap Int
genMap [] = empty
genMap (x:xs) = alter f x (genMap xs)
    where f Nothing = Just 1
          f (Just x) = Just (x + 1)

count :: IntMap Int -> [Int] -> Int
count _ [] = 0
count m (x:xs) = (x * v) + count m xs
    where v = findWithDefault 0 x m
