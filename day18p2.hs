import Data.Tuple (swap)
import Data.Array
import Data.Maybe
import Control.Monad (join)
import GHC.Arr (Array(Array))
import Data.List (intercalate)

main = do
    contents <- readFile "inputs/day18.txt"
    let bytes :: [(Int, Int)] = map ((swap . read) . (\x -> "(" ++ x ++ ")")) (lines contents)
        grid = listArray ((0, 0), (70, 70)) (repeat '.') // map (, '#') (take 1024 bytes)
        start = listArray (bounds grid) (repeat Nothing) // [((0, 0), Just 0)] :: Array (Int, Int) (Maybe Int)
    --print $ iter grid start
    --putStrLn $ display grid
    print bytes

connected bytes n = iter grid start
    where grid = listArray ((0, 0), (70, 70)) (repeat '.') // map (, '#') (take n bytes)
          start = listArray (bounds grid) (repeat Nothing) // [((0, 0), Just 0)]


iter grid paths
    | isJust (paths ! (70, 70)) = case paths ! (70, 70) of Just x -> Just x
    | otherwise = let f i = if grid ! i == '.' then (i, minNear paths i) else (i, Nothing)
                      new = array (bounds grid) $ map f $ range $ bounds grid
                  in if new == paths then Nothing else iter grid new

near (y, x) = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

minNear paths i@(y, x) = case mapMaybe (join . (paths !?)) (near i) of
    [] -> paths ! i
    l -> case paths ! i of
        Just x -> Just $ minimum $ x : map (+1) l
        Nothing -> Just $ minimum $ map (+1) l

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
    | inRange (bounds a) i = Just (a ! i)
    | otherwise = Nothing

display :: Array (Int, Int) Char -> String
display grid = intercalate "\n" (map (\y -> map (\x -> grid ! (y, x)) [0..snd (snd $ bounds grid)]) [0..fst (snd $ bounds grid)])