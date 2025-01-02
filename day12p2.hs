import Data.Array
import Data.IntMap.Strict (IntMap)
import Data.Graph (edges)

main = do
    contents <- readFile "inputs/day12.txt"
    let grid = listArray ((0, 0), (length (lines contents) - 1, length (head $ lines contents) - 1)) $ process contents
        start = listArray (bounds grid) [0..]
        end = fill grid start
    print $ sum $ map (value grid end) $ range (bounds grid)

process :: [Char] -> [Char]
process ('\n':xs) = process xs
process (x:xs) = x : process xs

get :: Array (Int, Int) Char -> (Int, Int) -> Char
get a i
    | not (inRange (bounds a) i) = '.'
    | otherwise = a ! i

fill :: Array (Int, Int) Char -> Array (Int, Int) Int -> Array (Int, Int) Int
fill grid old = let new = listArray (bounds grid) $ map (fillCell grid old) $ range $ bounds grid
                in if new == old then new else fill grid new

fillCell :: Array (Int, Int) Char -> Array (Int, Int) Int -> (Int, Int) -> Int
fillCell grid old (y, x) = minimum $ map (getCell grid old (grid ! (y, x))) [
        (y, x),
        (y + 1, x),
        (y - 1, x),
        (y, x + 1),
        (y, x - 1)
    ]

getCell :: Array (Int, Int) Char -> Array (Int, Int) Int -> Char -> (Int, Int) -> Int
getCell grid a c i
    | not (inRange (bounds a) i) = maxBound
    | grid ! i == c = a ! i
    | otherwise = maxBound

--counter :: Array (Int, Int) Int -> IntMap Int -> Int -> IntMap Int
--counter grid old value
--    | value `member` old = 

value grid areas i@(y, x) = let perimeter = findPerimeter grid areas i
                            in case perimeter of
                                0 -> 0
                                x -> x * length (filter (== areas ! i) $ elems areas)

findPerimeter grid areas i@(y, x) = length $ filter check edges
    where check (j, k, l) = grid !? j /= grid !? i && (grid !? k /= grid !? i || grid !? l == grid !? i)
          edges = [
            ((y + 1, x), (y, x + 1), (y + 1, x + 1)),
            ((y - 1, x), (y, x - 1), (y - 1, x - 1)),
            ((y, x + 1), (y - 1, x), (y - 1, x + 1)),
            ((y, x - 1), (y + 1, x), (y + 1, x - 1))
            ]


(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
    | inRange (bounds a) i = Just (a ! i)
    | otherwise = Nothing