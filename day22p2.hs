import Data.Bits (xor)
import Data.List (iterate', subsequences, maximumBy)
import Data.Ix
import qualified Data.Map.Strict as M
import Data.Set (fromList, toList)

main = do
    contents <- lines <$> readFile "inputs/day22.txt"
    let priceLists = map (prices . read) contents
        --patterns = filter viable $ map listify $ range ((-9, -9, -9, -9), (9, 9, 9, 9))
        scoreMap = map scores priceLists
        patterns = toList $ fromList $ concatMap M.keys scoreMap

    print $ length patterns
    print $ take 10 patterns
    --print $ maximum $ map (\pat -> sum $ map (value pat) priceLists) $ take 10 patterns

    --print $ maximumBy (\x y -> compare (sum x) (sum y)) $ map (\pat -> map (\m -> M.findWithDefault 0 pat m) scoreMap) patterns
    print $ maximum $ map (\pat -> sum $ map (\m -> M.findWithDefault 0 pat m) scoreMap) patterns



    --print $ sum $ map (calc iter . read) contents
    --mapM (print . take 4 . reverseBin) $ take 20 $ iterate iter 1

viable :: [Int] -> Bool
viable l@[a, b, c, d] = all ((\s -> s < 10 && s > (-10)) . sum) (subsequences l)

prices :: Int -> [Int]
prices x = take 2001 $ map (`mod` 10) $ iterate' iter x

listify (a, b, c, d) = [a, b, c, d]

value :: [Int] -> [Int] -> Int
value _ [_, _, _, _] = 0
value pattern (a:rest@(b:c:d:e:xs))
    | pattern == [b - a, c - b, d - c, e - d] = e
    | otherwise = value pattern rest

calc f x = iterate f x !! 2000

iter :: Int -> Int
iter = mp (* 2048) . mp (`div` 32) . mp (* 64)

mp f x = (x `xor` y) `mod` 16777216
    where y = f x

reverseBin :: Int -> String
reverseBin 0 = ""
reverseBin x = show (fromEnum $ odd x) ++ reverseBin (x `div` 2)

scores :: [Int] -> M.Map (Int, Int, Int, Int) Int
scores [_, _, _, _] = M.empty
scores (a:rest@(b:c:d:e:xs)) = M.insert (b - a, c - b, d - c, e - d) e $ scores rest

