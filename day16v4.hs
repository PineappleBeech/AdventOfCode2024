import Prelude hiding (Left, Right)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Array
import Data.List (intercalate)
import Debug.Trace (traceShow, trace)
import Data.Maybe (maybeToList, isJust)

data Direction = Up | Down | Left | Right deriving (Eq, Ord, Read, Show, Ix)

type Pos = (Int, Int)
type Head = (Pos, Direction)

type Path = (Int, Head, Set Pos)

type Grid = Array (Int, Int) Char
type Grid3 = Array Head (Maybe Int)

main = do
    contents <- readFile "inputs/day16.txt"
    let grid =  listArray ((0, 0), (length (lines contents) - 1, length (head $ lines contents) - 1)) $ concat $ lines contents
        start = findArray 'S' grid
        end = findArray 'E' grid
        initial = make3D grid // [((start, Right), Just 0)]
        mid = iter grid initial

    --print $ findPath grid end (0, (start, Right), Set.empty)
    print (mid ! (end, Right))
    --writeFile "inputs/day16temp.txt" (show mid)
    --print $ filter (\(i, a) -> isJust a) $ assocs initial

main2 = do
    mid :: Grid3 <- read <$> readFile "inputs/day16temp.txt"
    contents <- readFile "inputs/day16ex.txt"
    let grid =  listArray ((0, 0), (length (lines contents) - 1, length (head $ lines contents) - 1)) $ concat $ lines contents
        start = findArray 'S' grid
        end = findArray 'E' grid
        
    print (mid ! (end, Right))

extend :: Grid -> Path -> Set Path
extend grid (value, (pos, dir), visited)
    = Set.fromList $ filter (\(_, (p, d), v) -> grid ! p /= '#' && p `Set.notMember` v) [
        (value + 1, (move dir pos, dir), newVisited),
        (value + 1001, (move (rotate dir) pos, rotate dir), newVisited),
        (value + 1001, (move (opposite $ rotate dir) pos, opposite $ rotate dir), newVisited)
    ]
    where newVisited = Set.insert pos visited

findPath :: Grid -> Pos -> Path -> Int
findPath grid end start = f $ Set.singleton start
    where f paths = let (path@(value, (pos, _), _), new) = Set.deleteFindMin paths
                    in if pos == end
                        then value
                        else f (Set.union new $ extend grid path)

extend' :: Grid -> Path -> [Path]
extend' grid (value, (pos, dir), visited)
    = filter (\(_, (p, d), v) -> grid ! p /= '#' && p `Set.notMember` v) [
        (value + 1, (move dir pos, dir), visited),
        (value + 1001, (move (rotate dir) pos, rotate dir), newVisited),
        (value + 1001, (move (opposite $ rotate dir) pos, opposite $ rotate dir), newVisited)
    ]
    where newVisited = Set.insert pos visited

make3D :: Grid -> Grid3
make3D grid = listArray (((y1, x1), Up), ((y2, x2), Right)) (repeat Nothing)
    where ((y1, x1), (y2, x2)) = bounds grid

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
    | inRange (bounds a) i = Just (a ! i)
    | otherwise = Nothing

iter :: Grid -> Grid3 -> Grid3
iter grid paths =
    let f i = if grid ! fst i /= '#' then (i, minNear paths i) else (i, Nothing)
        new = array (bounds paths) $ map f $ range $ bounds paths
    in if trace "Iter" (new == paths) then paths else iter grid new

minNear :: Grid3 -> Head -> Maybe Int
minNear grid (pos@(y, x), dir) = case nearby of
        [] -> Nothing
        x -> Just $ minimum x
    where nearby = do
            (p, weight) <- [((move (opposite dir) pos, dir), 1), ((pos, rotate dir), 1000), ((pos, opposite (rotate dir)), 1000), ((pos, dir), 0)]
            v <- maybeToList (grid !? p) >>= maybeToList
            return (v + weight)

display :: Array (Int, Int) Char -> String
display grid = intercalate "\n" (map (\y -> map (\x -> grid ! (y, x)) [0..snd (snd $ bounds grid)]) [0..fst (snd $ bounds grid)])

findArray :: (Ix a, Eq b) => b -> Array a b -> a
findArray e a = fst $ head $ filter ((== e) . snd) $ assocs a

opposite Up = Down
opposite Down = Up
opposite Right = Left
opposite Left = Right

rotate Up = Left
rotate Left = Down
rotate Down = Right
rotate Right = Up

move :: Direction -> Pos -> Pos
move Up (y, x) = (y - 1, x)
move Down (y, x) = (y + 1, x)
move Right (y, x) = (y, x + 1)
move Left (y, x) = (y, x - 1)