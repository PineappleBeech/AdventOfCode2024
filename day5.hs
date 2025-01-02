import Text.Parsec
import Data.Set (Set, member, fromList)

main = do
    content <- readFile "inputs/day5.txt"
    let Right (pairs, sequences) = parse parser "" content
        pairSet = fromList pairs
    print $ sum $ map (value pairSet) sequences

number = do
    tens <- digit
    units <- digit
    return (read [tens] * 10 + read [units])

pair = do
    a <- number
    char '|'
    b <- number
    endOfLine
    return (a, b)

list = do
    a <- number `sepBy` char ','
    endOfLine
    return a

parser = do
    pairs <- many pair
    endOfLine
    sequences <- many list
    return (pairs, sequences)

test :: Set (Int, Int) -> [Int] -> Bool
test _ [] = True
test s (x:xs)
    | any ((`member` s) . (, x)) xs = False
    | otherwise = test s xs

value :: Set (Int, Int) -> [Int] -> Int
value s x
    | test s x = x !! (length x `div` 2)
    | otherwise = 0