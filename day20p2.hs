import Data.Array
import Data.Maybe (maybeToList, isNothing)
import Control.Monad (when, guard)


data Direction = Up | Down | West | East deriving (Eq, Ord, Show)

opposite :: Direction -> Direction
opposite Up = Down
opposite Down = Up
opposite East = West
opposite West = East

rotate :: Direction -> Direction
rotate Up = East
rotate East = Down
rotate Down = West
rotate West = Up

main = do
    contents <- readFile "inputs/day20.txt"
    let grid = listArray ((0, 0), (length (lines contents) - 1, length (head $ lines contents) - 1)) $ concat $ lines contents
        path = listArray (bounds grid) (repeat Nothing) // findPath grid
    print $ length $ find path

find grid = do
    pos <- range (bounds grid)
    old <- maybeToList (grid ! pos)
    newPos <- within 20 pos
    new <- maybeToList (grid !? newPos) >>= maybeToList
    guard (new >= old + 100 + distance pos newPos)

move :: (Num a, Num b) => Direction -> (a, b) -> (a, b)
move Up (y, x) = (y - 1, x)
move Down (y, x) = (y + 1, x)
move West (y, x) = (y, x - 1)
move East (y, x) = (y, x + 1)

findArray :: (Ix a, Eq b) => b -> Array a b -> a
findArray e a = fst $ head $ filter ((== e) . snd) $ assocs a

findPath :: Array (Int, Int) Char -> [((Int, Int), Maybe Int)]
findPath grid = zipWith (\i pos -> (pos, Just i)) [0..] (start : f start dir)
    where start = findArray 'S' grid
          dir = head $ filter ( (== '.').(!) grid . (`move` start)) [Up, Down, East, West]
          f pos dir = let newDir = head $ filter ((/= '#') . (!) grid . (`move` pos)) [dir, rotate dir, opposite $ rotate dir]
                      in case grid ! move newDir pos of
                        'E' -> [pos, move newDir pos]
                        '.' -> pos : f (move newDir pos) newDir

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
    | inRange (bounds a) i = Just (a ! i)
    | otherwise = Nothing

within n (y, x) = do
    dy <- [-n..n]
    dx <- [-(n - abs dy)..(n - abs dy)]
    return (y + dy, x + dx)

distance (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)