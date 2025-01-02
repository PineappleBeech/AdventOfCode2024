import Text.Parsec
main = do
    contents <- readFile "inputs/day14.txt"
    let robots = force $ parse parser "" contents
    print $ value $ mconcat $ map (sort . move) robots

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

move ((px, py), (vx, vy)) = ((px + 100 * vx) `mod` fst gridSize, (py + 100 * vy) `mod` snd gridSize)

sort pos@(x, y)
    | x < fst quadrants && y < snd quadrants = ([pos], [], [], [])
    | x > fst quadrants && y < snd quadrants = ([], [pos], [], [])
    | x < fst quadrants && y > snd quadrants = ([], [], [pos], [])
    | x > fst quadrants && y > snd quadrants = ([], [], [], [pos])
    | otherwise = ([], [], [], [])

value (a, b, c, d) = product [length a, length b, length c, length d]
