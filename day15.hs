import Data.Array
import Control.Monad.State
import Data.Foldable (find)

main = do
    contents <- readFile "inputs/day15.txt"
    let l = lines contents
        first = takeWhile (/= "") l
        grid = listArray ((0, 0), (length first - 1, length (head first) - 1)) $ concat first
        instructions = concat $ tail $ dropWhile (/= "") l
        end = execState (mapM iter instructions) grid


    print $ value end

iter :: Char -> State (Array (Int, Int) Char) ()
iter x = do
    grid <- get
    let pos = fst $ force $ find ((== '@') . snd) $ assocs grid
    case grid ! move pos x of
        '#' -> return ()
        '.' -> modify (// [(pos, '.'), (move pos x, '@')])
        'O' -> case boxspace grid (`move` x) (move pos x) of
            Just end -> modify (// [(pos, '.'), (move pos x, '@'), (end, 'O')])
            Nothing -> return ()

move :: (Int, Int) -> Char -> (Int, Int)
move (y, x) '^' = (y - 1, x)
move (y, x) 'v' = (y + 1, x)
move (y, x) '<' = (y, x - 1)
move (y, x) '>' = (y, x + 1)

boxspace :: Array (Int, Int) Char -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Maybe (Int, Int)
boxspace grid f pos = case grid ! pos of
    '.' -> Just pos
    'O' -> boxspace grid f (f pos)
    '#' -> Nothing

force (Just x) = x

value :: Array (Int, Int) Char -> Int
value grid = sum $ map (f . fst) $ filter ((== 'O') . snd) $ assocs grid
    where f (y, x) = y * 100 + x