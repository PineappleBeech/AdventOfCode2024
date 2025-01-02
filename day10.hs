import Data.Array
import Data.Set (singleton, Set, empty, unions, size)
import Data.Maybe (isNothing)

main = do
    contents <- readFile "inputs/day10.txt"
    let grid = toArray $ map (map (\x -> read [x])) $ lines contents
        --start = mapArray (\(a, i) -> if a ! i == 9 then 1 else 0) grid
        --end = foldl (iter grid) start [8,7..0]
    print $ sum $ map (size . check grid) $ filter ((== 0) . (grid !)) $ indices grid

toArray :: [[Int]] -> Array (Int, Int) Int
toArray grid = listArray ((0, 0), (height - 1, width - 1)) $ concat grid
    where height = length grid
          width = length $ head grid

mapArray :: Ix i => ((Array i a, i) -> b) -> Array i a -> Array i b
mapArray f a = array (bounds a) (map (\i -> (i, f (a, i))) $ range (bounds a))

iter :: Array (Int, Int) Int -> Array (Int, Int) Int -> Int -> Array (Int, Int) Int
iter m a n = mapArray f a
    where f (a, (y, x))
            | m ! (y, x) == n = sum $ map (sum . (a !?)) $ filter ((== Just (n + 1)) . (m !?)) [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
            | otherwise = 0

iter' :: Array (Int, Int) Int -> Array (Int, Int) Int -> Int -> Array (Int, Int) Int
iter' m a n = mapArray f a
    where f (a, (y, x)) = n --sum $ map (sum . (a !?)) $ filter ((== Just (n + 1)) . (m !?)) [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
    | inRange (bounds a) i = Just (a ! i)
    | otherwise = Nothing

check :: Array (Int, Int) Int -> (Int, Int) -> Set (Int, Int)
check a i@(y, x)
    | n == Just 9 = singleton i
    | isNothing n = empty
    | otherwise = let next = fmap (+1) n
        in unions $ map (check a) $ filter ((== next) . (a !?)) [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
    where n = a !? i