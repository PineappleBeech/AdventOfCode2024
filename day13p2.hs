import Text.Parsec hiding (try)

data Game = Game (Int, Int)  (Int, Int)  (Int, Int) deriving Show

main = do
    contents <- readFile "inputs/day13.txt"
    let games = force $ parse parser "" contents
    print $ sum $ map (solve . modify) games

parser = many machine

number :: Parsec String u Int
number = do
    s <- many digit
    return (read s)

machine = do
    string "Button A: X+"
    ax <- number
    string ", Y+"
    ay <- number
    string "\nButton B: X+"
    bx <- number
    string ", Y+"
    by <- number
    string "\nPrize: X="
    x <- number
    string ", Y="
    y <- number
    string "\n\n"
    return $ Game (x, y) (ax, ay) (bx, by)

force (Right a) = a

test :: Game -> (Int, Int) -> Bool
test (Game (x, y) (ax, ay) (bx, by)) (a, b) = x == (a * ax + b * bx) && y == (a * ay + b * by)

try :: Game -> (Int, Int) -> [Int]
try game@(Game (x, y) (ax, ay) (bx, by)) (a, b)
    | test game (a, b) = 3 * a + b : try game (a + 1, b)
    | a * ax + b * bx < x = try game (a + 1, b)
    | b > 100 = []
    | otherwise = try game (0, b + 1)

value game = case try game (0, 0) of
    [] -> 0
    l -> minimum l

linear (Game (x, y) (ax, ay) (bx, by)) = ax * by == ay * bx

-- solve ax * a + bx * b = 10000000000000
--       ay * a + by * b = 10000000000000
simplify :: Game -> (Game, Int)
simplify game@(Game (x, y) (ax, ay) (bx, by)) =
    let det = ax * by - bx * ay
        a = (by - bx) * 10000000000000 `div` det
        b = (- ay + ax) * 10000000000000 `div` det
    in (Game (x - a * ax - b * bx + 10000000000000, y - a * ay - b * by + 10000000000000) (ax, ay) (bx, by), 3 * a + b)

value' game = let (new, extra) = simplify game in case value new of
    0 -> 0
    x -> x + extra

modify (Game (x, y) (ax, ay) (bx, by)) = Game (x + 10000000000000, y + 10000000000000) (ax, ay) (bx, by)

solve game@(Game (x, y) (ax, ay) (bx, by)) =
    let det = ax * by - bx * ay
        adet = x * by - y * bx
        bdet = - x * ay + y * ax
    in (if adet `mod` det == 0 && bdet `mod` det == 0 then 3 * (adet `div` det) + (bdet `div` det) else 0)
