import Control.Monad.State
import qualified Data.Map as M
import Debug.Trace (traceShowId, traceShow)

main = do
    contents <- lines <$> readFile "inputs/day21.txt"
    let codes = map value contents
    print $ sum codes

value :: String -> Int
value = uncurry (*) . value'

iter' = filterMin . iter

iter :: String -> [String]
iter s = map concat $ evalStateT (mapM press s) (getPos 'A')

press :: Char -> StateT (Int, Int) [] [Char]
press key = do
    pos <- get
    let new = getPos key
        v = if fst new > fst pos then 'v' else '^'
        h = if snd new > snd pos then '>' else '<'
        vkeys = replicate (abs (fst new - fst pos)) v
        hkeys = replicate (abs (snd new - snd pos)) h
        vfirst = vkeys ++ hkeys ++ "A"
        hfirst = hkeys ++ vkeys ++ "A"
        keys = case (fst pos == 3 && snd new == 0, fst new == 3 && snd pos == 0, null hkeys || null vkeys) of
            (_, _, True) -> [vfirst]
            (False, False, False) -> [vfirst, hfirst]
            (True, False, False) -> [vfirst]
            (False, True, False) -> [hfirst]

    keys' <- lift keys

    put (getPos key)
    return keys'

calc :: (Int, String) -> State (M.Map (Int, String) Int) Int
calc (0, _) = return 1
calc (depth, s) = do
    value <- mapM (mapM (memo calc . (depth - 1, )) . split) (iter s)
    return $ minimum $ map sum value

split :: String -> [String]
split [] = []
split s = (next ++ "A") : split (tail rest)
    where (next, rest) = span (/= 'A') s

memo :: Ord a => (a -> State (M.Map a b) b) -> (a -> State (M.Map a b) b)
memo f a = do
    m <- get
    case M.lookup a m of
        Nothing -> let (b, new) = runState (f a) m in put new >> modify (M.insert a b) >> return b
        Just b -> return b

eval :: (a -> State (M.Map a b) b) -> a -> b
eval f a = evalState (f a) M.empty

value' :: String -> (Int, Int)
value' s = (read $ take 3 s , eval calc (27, s))

getPos :: (Num a, Num b) => Char -> (a, b)
getPos 'A' = (3, 2)
getPos '^' = (3, 1)
getPos '<' = (4, 0)
getPos 'v' = (4, 1)
getPos '>' = (4, 2)
getPos '0' = (3, 1)
getPos '1' = (2, 0)
getPos '2' = (2, 1)
getPos '3' = (2, 2)
getPos '4' = (1, 0)
getPos '5' = (1, 1)
getPos '6' = (1, 2)
getPos '7' = (0, 0)
getPos '8' = (0, 1)
getPos '9' = (0, 2)

filterMin :: [[a]] -> [[a]]
filterMin l = filter ((== m) . length) l
    where m = minimum $ map length l