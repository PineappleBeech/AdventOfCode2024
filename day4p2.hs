import Data.List (transpose)
main = do
    contents <- readFile "inputs/day4.txt"
    let grid = lines contents
    print $ sum $ map (sum . map cross . group3) [grid, rot grid, rot (rot grid), rot (rot (rot grid))]

rot = map reverse . transpose

horizontal (x:m:a:s:xs)
    | x == 'X' && a == 'A' && m == 'M' && s == 'S' = 1 + horizontal xs
    | otherwise = horizontal (m:a:s:xs)
horizontal _ = 0

diagonal ([_, _, _], [_, _, _], [_, _, _], [_, _, _]) = 0
diagonal l@((x:_), (_:m:_), (_:_:a:_), (_:_:_:s:_))
    | x == 'X' && a == 'A' && m == 'M' && s == 'S' = 1 + diagonal (map4 tail l)
    | otherwise = diagonal (map4 tail l)

group [_,_,_] = []
group (a:b:c:d:l) = (a,b,c,d) : group (b:c:d:l)

map4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4 f (a, b, c, d) = (f a, f b, f c, f d)

calc l = sum (map horizontal l) + sum (map diagonal (group l))

cross ([_, _], [_, _], [_, _]) = 0
cross l@((m1:_:s1:_), (_:a:_), (m2:_:s2:_))
    | m1 == 'M' && m2 == 'M' && a == 'A' && s1 == 'S' && s2 == 'S' = 1 + cross (map3 tail l)
    | otherwise = cross (map3 tail l)

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (a, b, c) = (f a, f b, f c)

group3 [_,_] = []
group3 (a:b:c:l) = (a,b,c) : group3 (b:c:l)