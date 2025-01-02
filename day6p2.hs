import Data.Array
import Data.List (findIndex, transpose)
import Data.Set (size, fromList, Set, empty, member, insert)

data Direction = Up | Down | West | East deriving (Eq, Show, Ord)

main = do
    contents <- readFile "inputs/day6.txt"
    let l = lines contents
        width = length $ head l
        height = length l
        grid = listArray ((1,1), (height, width)) $ process contents
        start = findStart l
        p = path grid start Up
        l2 = map (\(x,y,dir) -> move dir (x,y)) $ filter (check grid (uncurry (,,Up) start)) p

    print l2
    print $ size $ fromList l2

process :: String -> String
process [] = []
process ('\n':xs) = process xs
process ('^':xs) = '.' : process xs
process (x:xs) = x : process xs

findStart :: [String] -> (Int, Int)
findStart l = case (findIndex (elem '^') l, findIndex (elem '^') $ transpose l) of
    (Just y, Just x) -> (y + 1, x + 1)

path :: Array (Int, Int) Char -> (Int, Int) -> Direction -> [(Int, Int, Direction)]
path grid pos dir
    | not (inRange (bounds grid) next) = [uncurry (,,dir) pos]
    | grid ! next == '#' = path grid pos $ rotate dir
    | otherwise = uncurry (,,dir) pos : path grid next dir
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

check :: Array (Int, Int) Char -> (Int, Int, Direction) -> (Int, Int, Direction) -> Bool
check grid start (y, x, dir)
    | not (inRange (bounds grid) next) = False
    | otherwise = checkCycle (grid // [(next, '#')]) empty start

    where pos = (y, x)
          next = move dir pos

checkCycle :: Array (Int, Int) Char -> Set (Int, Int, Direction) -> (Int, Int, Direction) -> Bool
checkCycle grid visited current@(y, x, dir)
    | current `member` visited = True
    | not (inRange (bounds grid) next) = False
    | grid ! next == '#' = checkCycle grid (insert current visited) (y, x, rotate dir)
    | otherwise = checkCycle grid (insert current visited) $ uncurry (,,dir) $ move dir pos
    where pos = (y, x)
          next = move dir pos
