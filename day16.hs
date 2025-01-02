import Data.Array
import qualified Data.Map.Strict as M
import Control.Monad.State

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
    --print $ eval (search' grid) (start,  East)
    print $ minEval (search2 grid) (start,  East) 0

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
    | otherwise = minimum <$> mapM look [facing, rotate facing, opposite $ rotate facing]
    where look dir
            | dir == facing = if grid ! move dir pos /= '#' then (1 +) <$> memo (search' (grid // [(pos, '#')])) (move dir pos, dir) else return large
            | dir == opposite facing = return large
            | otherwise = if grid ! move dir pos /= '#' then (1001 +) <$> memo (search' (grid // [(pos, '#')])) (move dir pos, dir) else return large

search2 grid (pos, facing) distance
    | grid ! pos == 'E' = return distance
    | otherwise = minimum <$> mapM look [facing, rotate facing, opposite $ rotate facing]
    where look dir
            | dir == facing = if grid ! move dir pos /= '#' then minMemo (search2 (grid // [(pos, '#')])) (move dir pos, dir) (distance + 1) else return large
            | dir == opposite facing = return large
            | otherwise = if grid ! move dir pos /= '#' then minMemo (search2 (grid // [(pos, '#')])) (move dir pos, dir) (distance + 1001) else return large


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

minMemo :: (Ord a, Ord v) => (a -> v -> State (M.Map a (v, b)) b) -> (a -> v -> State (M.Map a (v, b)) b)
minMemo f a v = do
    m <- get
    case M.lookup a m of
        Nothing -> let (b, new) = runState (f a v) m in put new >> modify (M.insert a (v, b)) >> return b
        Just (oldv, b) | oldv > v -> let (b, new) = runState (f a v) m in put new >> modify (M.insert a (v, b)) >> return b
        Just (v, b) -> return b

minEval :: Ord a => (a -> v -> State (M.Map a (v, b)) b) -> a -> v -> b
minEval f a v = evalState (f a v) M.empty

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
