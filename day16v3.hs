import Data.Array
import Data.Maybe
import Control.Monad (join)
import Data.List (intercalate)

main = do
    contents <- readFile "inputs/day16p2.txt"
    let grid =  listArray ((0, 0), (length (lines contents) - 1, length (head $ lines contents) - 1)) $ concat $ lines contents
        start = findArray 'P' grid
        end = findArray 'E' grid
        initial = listArray (bounds grid) (repeat Nothing) // [(start, Just 0)]
        mid = spread grid end initial
        mid' = spread' grid end initial 516
        reduced = reduce end mid'
    putStrLn $ display $ fmap (\x -> if isJust x then '.' else '#') mid'
    putStrLn $ display $ fmap (\x -> if isJust x then '.' else '#') reduced
    print $ length $ filter (isJust . snd) $ assocs reduced

reduce end paths = let new =  paths // mapMaybe f (assocs paths)
                   in if paths == new then paths else reduce end new
    where f (_, Nothing) = Nothing
          f (pos, Just value)
            | pos == end = Nothing
            | all (< value - 20) $ catMaybes $ mapMaybe (paths !?) $ near pos = Just (pos, Nothing)
            | otherwise = Nothing

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
    | inRange (bounds a) i = Just (a ! i)
    | otherwise = Nothing

findArray :: (Ix a, Eq b) => b -> Array a b -> a
findArray e a = fst $ head $ filter ((== e) . snd) $ assocs a

spread grid end paths
    | isJust (paths ! end) = case paths ! end of Just x -> paths
    | otherwise = let f i = if grid ! i `elem` "SPE" then (i, minNear paths i) else (i, Nothing)
                      new = array (bounds grid) $ map f $ range $ bounds grid
                  in if new == paths then undefined else spread grid end new

spread' grid end paths 0 = paths
spread' grid end paths n
    | isJust (paths ! end) = case paths ! end of Just x -> paths
    | otherwise = let f i = if grid ! i `elem` "SPE" then (i, minNear paths i) else (i, Nothing)
                      new = array (bounds grid) $ map f $ range $ bounds grid
                  in spread' grid end new (n - 1)

near (y, x) = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

minNear paths i@(y, x) = case mapMaybe (join . (paths !?)) (near i) of
    [] -> paths ! i
    l -> case paths ! i of
        Just x -> Just $ minimum $ x : map (+1) l
        Nothing -> Just $ minimum $ map (+1) l

display :: Array (Int, Int) Char -> String
display grid = intercalate "\n" (map (\y -> map (\x -> grid ! (y, x)) [0..snd (snd $ bounds grid)]) [0..fst (snd $ bounds grid)])
