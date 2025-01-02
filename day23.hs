import qualified Data.Map as M
import Data.Map ((!))
import Data.List (sort)
import Control.Monad (guard)
import Data.Set (fromList, size)

main = do
    contents <- lines <$> readFile "inputs/day23.txt"
    let pairs = map (\s -> (take 2 s, drop 3 s)) contents
    let m = genMap pairs
    let triples = findTriples m
    print $ size $ fromList triples

genMap :: [(String, String)] -> M.Map String [String]
genMap [] = M.empty
genMap ((a, b):xs) = M.alter (f b) a $ M.alter (f a) b $ genMap xs
    where f value Nothing = Just [value]
          f value (Just old) = Just (value : old)

findTriples :: M.Map String [String] -> [[String]]
findTriples m = do
    a <- filter ((== 't') . head) $ M.keys m
    b <- m ! a
    c <- m ! b
    guard (a `elem` (m ! c))
    return $ sort [a, b, c]