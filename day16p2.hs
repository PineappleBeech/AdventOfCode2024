import Data.Array
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.List (intercalate)

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
    contents <- readFile "inputs/day16.txt"
    let grid = listArray ((0, 0), (length (lines contents) - 1, length (head $ lines contents) - 1)) $ concat $ lines contents
        start = findArray 'S' grid
        end = findArray 'E' grid
        grids = iterate iter' (fmap (\x -> if x == 'S' then ('S', Just 0) else (x, Nothing)) grid)
        width = snd $ snd $ bounds grid
        paths = fmap process (grids !! 105) // [(start, 'S')]
    --print $ eval (search' grid) (start,  East)
    --forM (zip grids [0..]) (\(g, i) -> putStrLn (display g) >> print i >> getLine)
    --print $ (grids !! 7) ! (2, width - 1)
    --print $ (grids !! 105) ! (1, width - 2)
    --print (grids !! 7)
    --putStrLn $ display $ fmap fst (grids !! 104)
    --putStrLn $ display $ fmap fst (grids !! 105)
    --print $ (grids !! 105) ! (1, width - 2)
    
    --putStrLn $ display $ fmap fst (grids !! 6)
    --putStrLn $ display $ fmap fst (grids !! 7)
    --print $ (grids !! 7) ! (2, width - 1)
    --print (515 + 1000 * (105 - 1) + 1)
    print $ follow paths start 515
    print $ follow paths start 514
    print $ follow paths start 516
    print $ display paths

follow :: Array (Int, Int) Char -> (Int, Int) -> Int -> Int
follow grid pos current
    | grid ! pos == 'E' = undefined
    | current < 0 = 0
    | otherwise = sum $ map look [Up, Down, East, West]
    where look dir
            | grid ! move dir pos == '.' = follow (grid // [(pos, 'S')]) (move dir pos) (current - 1)
            | otherwise = 0

process ('S', _) = '.'
process ('E', _) = 'E'
process (x, _) = '#'

iter :: Array (Int, Int) Char -> Array (Int, Int) Char
iter grid = let possible = do
                                pos <- assocs grid
                                dir <- [Up, Down, East, West]
                                return (pos, dir)
                check ((pos, c), dir) = c == '.' && grid ! move (opposite dir) pos == 'S'
                nodes = map (\((pos, c), dir) -> (pos, dir)) $ filter check possible
                extend (pos, dir)
                    | grid ! pos == '.' = (pos, 'S') : extend (move dir pos, dir)
                    | otherwise = []
            in grid // concatMap extend nodes

iter' :: Array (Int, Int) (Char, Maybe Int) -> Array (Int, Int) (Char, Maybe Int)
iter' grid = let possible = do
                                pos <- assocs grid
                                dir <- [Up, Down, East, West]
                                return (pos, dir)
                 check ((pos, (c, _)), dir) = c == '.' && fst (grid ! move (opposite dir) pos) == 'S'
                 nodes = map (\((pos, c), dir) -> (pos, dir)) $ filter check possible
                 extend n (pos, dir)
                    | fst (grid ! pos) == '.' = (pos, ('S', Just n)) : extend (n + 1) (move dir pos, dir)
                    | otherwise = []
            in grid // concatMap (\x@(pos, dir) -> extend ((force $ snd $ grid ! move (opposite dir) pos) + 1) (pos, dir)) nodes

iterE :: Array (Int, Int) (Char, Maybe Int) -> Array (Int, Int) (Char, Maybe Int)
iterE grid = let possible = do
                                pos <- assocs grid
                                dir <- [Up, Down, East, West]
                                return (pos, dir)
                 check ((pos, (c, _)), dir) = c == '.' && fst (grid ! move (opposite dir) pos) == 'S'
                 nodes = map (\((pos, c), dir) -> (pos, dir)) $ filter check possible
                 extend n (pos, dir)
                    | fst (grid ! pos) == '.' = (pos, ('S', Just n)) : extend (n + 1) (move dir pos, dir)
                    | otherwise = []
            in grid // concatMap (\x@(pos, dir) -> extend ((force $ snd $ grid ! move (opposite dir) pos) + 1) (pos, dir)) nodes

display :: Array (Int, Int) Char -> String
display grid = intercalate "\n" (map (\y -> map (\x -> grid ! (y, x)) [0..snd (snd $ bounds grid)]) [0..fst (snd $ bounds grid)])

force (Just x) = x

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
    | inRange (bounds a) i = Just (a ! i)
    | otherwise = Nothing





findArray :: (Ix a, Eq b) => b -> Array a b -> a
findArray e a = fst $ head $ filter ((== e) . snd) $ assocs a

large :: Int
large = maxBound `div` 2

search grid pos facing distance
    | grid ! pos == 'E' = distance
    | otherwise = minimum $ map look [facing, rotate facing, opposite $ rotate facing]
    where look dir
            | dir == facing = if grid ! move dir pos /= '#' then search (grid // [(pos, '#')]) (move dir pos) dir (distance + 1) else large
            | dir == opposite facing = large
            | otherwise = if grid ! move dir pos /= '#' then search (grid // [(pos, '#')]) (move dir pos) dir (distance + 1001) else large

search' grid (pos, facing)
    | grid ! pos == 'E' = return 0
    | otherwise = minimum <$> mapM look [Up, Down, West, East]
    where look dir
            | dir == facing = if grid ! move dir pos /= '#' then (1 +) <$> memo (search' (grid // [(pos, '#')])) (move dir pos, dir) else return large
            | dir == opposite facing = return large
            | otherwise = if grid ! move dir pos /= '#' then (1001 +) <$> memo (search' (grid // [(pos, '#')])) (move dir pos, dir) else return large

move Up (y, x) = (y - 1, x)
move Down (y, x) = (y + 1, x)
move West (y, x) = (y, x - 1)
move East (y, x) = (y, x + 1)

memo :: Ord a => (a -> State (M.Map a b) b) -> (a -> State (M.Map a b) b)
memo f a = do
    m <- get
    case M.lookup a m of
        Nothing -> let (b, new) = runState (f a) m in put new >> modify (M.insert a b) >> return b
        Just b -> return b

eval :: Ord a => (a -> State (M.Map a b) b) -> a -> b
eval f a = evalState (f a) M.empty

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Int -> State (M.Map Int Int) Int
fib' 0 = return 1
fib' 1 = return 1
fib' n = do
    a <- memo fib' (n - 1)
    b <- memo fib' (n - 2)
    return (a + b)
