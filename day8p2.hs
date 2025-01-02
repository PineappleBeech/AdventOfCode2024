import Data.List (findIndex, elemIndices, delete)
import Data.Ix (inRange)
import Data.Set (fromList, size)

main = do
    contents <- readFile "inputs/day8.txt"
    let grid = lines contents
        gridBounds = ((0, 0), (length grid - 1, length (head grid) - 1))
    print $ size $ fromList $ findAll grid gridBounds

chars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

findChar :: [String] -> Char -> [(Int, Int)]
findChar grid c = do
    line <- zip [0..] grid
    column <- elemIndices c $ snd line
    return (fst line, column)

permutate :: Eq a => [a] -> [(a, a)]
permutate l = do
    a <- l
    b <- delete a l
    return (a, b)

findAntinode :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAntinode ((y1, x1), old@(y2, x2)) = old : findAntinode (old, new)
    where new = (2 * y2 - y1, 2 * x2 - x1)

findAll :: [String] -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAll grid bounds = do
    c <- chars
    pair <- permutate $ findChar grid c
    takeWhile (inRange bounds) $ findAntinode pair