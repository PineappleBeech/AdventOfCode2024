import Text.Parsec

main = do
    contents <- readFile "inputs/day7.txt"
    let computations = map (forceParse line ) $ lines contents
    print $ sum $ map fst $ filter (uncurry test) computations

number :: Parsec String u Int
number = do
    a <- digit
    b <- many digit
    return $ read (a:b)


line = do
    target <- number
    char ':'
    values <- many (space >> number)
    return (target, values)

test :: Int -> [Int] -> Bool
test t [x] = t == x
test t (x:y:xs) = test t (x + y : xs) || test t (x * y : xs) || test t (read (show x ++ show y) : xs)

forceParse p s = case parse p "" s of
    Right v -> v
    Left e -> error "Cant Parse"
