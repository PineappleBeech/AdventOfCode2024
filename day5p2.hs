import Text.Parsec
import Data.Set (Set, member, fromList)
import qualified Data.Map as Map
import Data.Map ((!))

main = do
    content <- readFile "inputs/day5.txt"
    let Right (pairs, sequences) = parse parser "" content
        pairSet = fromList pairs
    print $ sum $ map (\x -> if test pairSet x then 0 else midpoint pairSet x) sequences

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

midpoint :: Set (Int, Int) -> [Int] -> Int
midpoint s l = m ! k
    where k = length l `div` 2
          m = Map.fromList $ map (\x -> (length $ filter ((`member` s) . (, x)) l, x)) l