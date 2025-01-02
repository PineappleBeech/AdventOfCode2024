{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
import qualified Data.Map as Map

type Map = Map.Map (Int, Int) Int

main = do
    contents <- readFile "inputs/day11.txt"
    let start = map read $ words contents
        m = foldl mapped Map.empty (map (, 75) start)
    --print $ sum $ map (\x -> simple (x, 25)) start
    print $ sum $ map (\x -> m Map.! (x, 75)) start

iter :: [Int] -> [Int]
iter [] = []
iter (0:xs) = 1 : iter xs
iter (x:xs)
    | even (length s) = let (a, b) = splitAt (length s `div` 2) s
                        in read a : read b : iter xs
    | otherwise = x * 2024 : iter xs
    where s = show x

simple :: (Int, Int) -> Int
simple (_, 0) = 1
simple (0, n) = simple (1, n - 1)
simple (x, n)
    | even (length s) = let (a, b) = splitAt (length s `div` 2) s
                        in simple (read a, n - 1) + simple (read b, n - 1)
    | otherwise = simple (x * 2024, n - 1)
    where s = show x

mapped :: Map -> (Int, Int) -> Map
mapped m t@(x, 0) = Map.insert t 1 m
mapped m t@(0, n) = case Map.member t m of
    True -> m
    False -> let new = mapped m (1, n - 1)
             in Map.insert t (new Map.! (1, n - 1)) new
mapped m t@(x, n) = case Map.member t m of
    True -> m
    False -> let s = show x
             in case even (length s) of
                True -> let (a, b) = splitAt (length s `div` 2) s
                            new = mapped m (read a, n - 1)
                            new2 = mapped new (read b, n - 1)
                        in Map.insert t (new2 Map.! (read a, n - 1) + new2 Map.! (read b, n - 1)) new2
                False -> let new = mapped m (x * 2024, n - 1)
                         in Map.insert t (new Map.! (x * 2024, n - 1)) new

m ! k = case m Map.!? k of
    Just v -> v
    Nothing -> error "here" 