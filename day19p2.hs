import Control.Monad (guard)
import Data.Text (unpack, pack, splitOn)
import qualified Data.Map.Strict as M
import Control.Monad.State
    ( guard, MonadState(put, get), modify, evalState, runState, State )
import Debug.Trace
import GHC.IO (unsafePerformIO)

main = do
    contents <- readFile "inputs/day19.txt"
    let l = lines contents
        patterns :: [String] = map unpack $ splitOn (pack ", ") $ pack $ head l
        patterns' = map fst $ filter (\(x, xs) -> 0 == check xs x) $ f patterns
        strings = drop 2 l

    --print $ sum $ map (check patterns) strings
    print $ valueAll patterns strings
    --print patterns
    --print patterns'
    --print (map (check patterns) patterns)

f [] = []
f (x:xs) = (x, xs) : map (fmap (x :)) (f xs)

patterns :: [String] = map unpack $ splitOn (pack ", ") $ pack $ head l
    where contents = unsafePerformIO $ readFile "inputs/day19.txt"
          l = lines contents

patterns' = map fst $ filter (\(x, xs) -> 0 == check xs x) $ f patterns

check patterns = length . find patterns

find :: [String] -> String -> [[String]]
find patterns s = do
    p <- patterns
    guard (checkPartial p s)
    if length p == length s
        then return [p]
        else  map (p :) (find patterns (drop (length p) s))

find' :: [String] -> String -> State (M.Map String [[String]]) [[String]]
find' patterns s = do
    let f = memo (find' patterns)
        l = filter (`checkPartial` s) patterns
        g p = do
                if length p == length s
                    then return [[p]]
                    else f (drop (length p) s) >>= return . map (p :)
    if null l
        then return []
        else mapM g l >>= return . concat

check' patterns = length . eval (find' patterns)

checkAll :: [String] -> [String] -> Int
checkAll patterns strings = sum $ map (length . trace "Done") $ evalState (mapM (find' patterns) strings) M.empty

memo :: (Ord a, Show b, Show a) => (a -> State (M.Map a b) b) -> (a -> State (M.Map a b) b)
memo f a = do
    m <- get
    case M.lookup a m of
        Nothing -> let (b, new) = runState (f a) m in put new >> modify (M.insert a b) >> return b
        Just b -> return b

eval :: Ord a => (a -> State (M.Map a b) b) -> a -> b
eval f a = evalState (f a) M.empty

findValue :: [String] -> String -> State (M.Map String Int) Int
findValue patterns s = do
    let f = memo (findValue patterns)
        l = filter (`checkPartial` s) patterns
        g p = do
                if length p == length s
                    then return 1
                    else f (drop (length p) s)
    if null l
        then return 0
        else mapM g l >>= return . sum

valueAll patterns strings = sum $ evalState (mapM (findValue patterns) strings) M.empty

checkPartial p s = and (zipWith (==) p s) && length p <= length s