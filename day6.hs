import Data.Array
import Data.List (findIndex, transpose)
import Data.Set (size, fromList)

data Direction = Up | Down | West | East

main = do
    contents <- readFile "inputs/day6.txt"
    let l = lines contents
        width = length $ head l
        height = length l
        grid = listArray ((1,1), (height, width)) $ process contents
        start = findStart l

    print $ size $ fromList $ path grid start Up

process :: String -> String
process [] = []
process ('\n':xs) = process xs
process ('^':xs) = '.' : process xs
process (x:xs) = x : process xs

findStart :: [String] -> (Int, Int)
findStart l = case (findIndex (elem '^') l, findIndex (elem '^') $ transpose l) of
    (Just y, Just x) -> (y + 1, x + 1)

path :: Array (Int, Int) Char -> (Int, Int) -> Direction -> [(Int, Int)]
path grid pos dir
    | not (inRange (bounds grid) next) = [pos]
    | grid ! next == '#' = path grid pos $ rotate dir
    | otherwise = pos : path grid next dir
    where next = move dir pos

move :: Direction -> (Int, Int) -> (Int, Int)
move Up (y, x) = (y-1, x)
move Down (y, x) = (y+1, x)
move West (y, x) = (y, x-1)
move East (y, x) = (y, x+1)

rotate :: Direction -> Direction
rotate Up = East
rotate East = Down
rotate Down = West
rotate West = Up