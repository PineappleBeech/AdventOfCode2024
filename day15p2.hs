import Data.Array
import Control.Monad.State
import Data.Foldable (find)
import Data.List (intercalate, nubBy)
import Data.Function (on)

main = do
    contents <- readFile "inputs/day15.txt"
    let l = lines contents
        first = takeWhile (/= "") l
        grid = listArray ((0, 0), (length first - 1, length (head first) * 2 - 1)) $ concatMap (concatMap double) first
        instructions = concat $ tail $ dropWhile (/= "") l
        end = execState (mapM iter instructions) grid


    print $ value end
    putStrLn $ display end

double :: Char -> String
double '#' = "##"
double 'O' = "[]"
double '.' = ".."
double '@' = "@."

iter :: Char -> State (Array (Int, Int) Char) ()
iter x = do
    grid <- get
    let pos = fst $ force $ find ((== '@') . snd) $ assocs grid
    case (grid ! move pos x, x `elem` "<>") of
        ('#', _) -> return ()
        ('.', _) -> modify (// [(pos, '.'), (move pos x, '@')])
        (_, True) -> case boxh grid (`move` x) (move pos x) of
            Nothing -> return ()
            Just l -> modify (// l) >> modify (// [(pos, '.'), (move pos x, '@')])
        (box, False) -> let left = if box == '[' then move pos x else move (move pos x) '<'
                        in case boxv grid (`move` x) left of
                            Nothing -> return ()
                            Just l -> modify (`update` l) >> modify (// [(pos, '.'), (move pos x, '@')])

move :: (Int, Int) -> Char -> (Int, Int)
move (y, x) '^' = (y - 1, x)
move (y, x) 'v' = (y + 1, x)
move (y, x) '<' = (y, x - 1)
move (y, x) '>' = (y, x + 1)

boxh :: Array (Int, Int) Char -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Maybe [((Int, Int), Char)]
boxh grid f pos = case grid ! next of
    '.' -> Just [(next, grid ! pos)]
    '#' -> Nothing
    _ -> ([(next, grid ! pos)] ++ ) <$> boxh grid f next
    where next = f pos

boxv :: Array (Int, Int) Char -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Maybe [((Int, Int), Char)]
boxv grid f pos = let left = pos
                      right = move left '>'
                      prepend = ([(left, '.'), (right, '.'), (f left, grid ! left), (f right, grid ! right)] ++ )
    in case (grid ! (f left), grid ! (f right)) of
        ('#', _) -> Nothing
        (_, '#') -> Nothing
        (']', '[') -> ((++) . prepend <$> boxv grid f (move (f left) '<')) <*> boxv grid f (move (f left) '>')
        ('[', ']') -> prepend <$> boxv grid f (f left)
        (']', _) -> prepend <$> boxv grid f (move (f left) '<')
        (_, '[') -> prepend <$> boxv grid f (move (f left) '>')
        (_, _) -> prepend <$> Just []

update a [] = a
update a (x@(pos, '.'):xs) = if not (any ((== pos) . fst) xs) then update a xs // [x] else update a xs
update a (x:xs) = update a xs // [x]


force (Just x) = x

value :: Array (Int, Int) Char -> Int
value grid = sum $ map (f . fst) $ filter ((== '[') . snd) $ assocs grid
    where f (y, x) = y * 100 + x

display :: Array (Int, Int) Char -> String
display grid = intercalate "\n" (map (\y -> map (\x -> grid ! (y, x)) [0..snd (snd $ bounds grid)]) [0..fst (snd $ bounds grid)])