import Text.Parsec
import Data.List (intercalate)
import Control.Monad (forM)
import Data.Bifunctor (Bifunctor(bimap))

main = do
    contents <- readFile "inputs/day14.txt"
    let robots = force $ parse parser "" contents
        x = head $ filter (\x -> x `mod` 103 == 1 && x `mod` 101 == 46) [1..]
    --putStrLn $ display robots
    --forM (iterate (bimap (map move) (+1)) (robots, 0)) (\(x, n) -> putStrLn (display x) >> print n >> getLine)
    --forM (iterate (bimap (map move) (+1)) (robots, 0)) iter
    putStrLn $ display $ map (moveBy x) robots
    print x

iter (x, n) = if check x then putStrLn (display x) >> print n >> getLine else return ""

force (Right a) = a

parser = many robot

number :: Parsec String u Int
number = do
    sign <- option ' ' $ char '-'
    digits <- many digit
    return (read (sign:digits) :: Int)

robot = do
    string "p="
    px <- number
    string ","
    py <- number
    string " v="
    vx <- number
    string ","
    vy <- number
    string "\n"
    return ((px, py), (vx, vy))

gridSize = (101, 103)
quadrants = (fst gridSize `div` 2, snd gridSize `div` 2)

move ((px, py), (vx, vy)) = (((px + vx) `mod` fst gridSize, (py + vy) `mod` snd gridSize), (vx, vy))

sort pos@(x, y)
    | x < fst quadrants && y < snd quadrants = ([pos], [], [], [])
    | x > fst quadrants && y < snd quadrants = ([], [pos], [], [])
    | x < fst quadrants && y > snd quadrants = ([], [], [pos], [])
    | x > fst quadrants && y > snd quadrants = ([], [], [], [pos])
    | otherwise = ([], [], [], [])

value (a, b, c, d) = product [length a, length b, length c, length d]

display :: [((Int, Int), (Int, Int))] -> String
display l =  intercalate "\n" $ map (\y -> map (\x -> if (x, y) `elem` positions then 'X' else '.') [0..fst gridSize - 1]) [0..snd gridSize - 1]
    where positions = map fst l

check :: [((Int, Int), (Int, Int))] -> Bool
check l = all checkPos' positions
    where positions = map fst l

checkPos :: (Int, Int) -> Bool
checkPos (x, y)
    | abs (x - fst quadrants) > 5 = True
    | abs (y - snd quadrants) > 5 = True
    | otherwise = False

checkPos' :: (Int, Int) -> Bool
checkPos' (x, y)
    | x < 5 && y < 5 = False
    | x >= fst gridSize - 5 && y < 5 = False
    | x < 5 && y >= snd gridSize - 5 = False
    | x >= fst gridSize - 5 && y >= snd gridSize - 5 = False
    | otherwise = True


time :: ((Int, Int), (Int, Int)) -> Int
time robot@(pos@(px, py), (vx, vy))
    | checkPos pos = 0
    | otherwise = 1 + time (move robot)

moveBy v ((px, py), (vx, vy)) = (((px + v * vx) `mod` fst gridSize, (py + v * vy) `mod` snd gridSize), (vx, vy))
