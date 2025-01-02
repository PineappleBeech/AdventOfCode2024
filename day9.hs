import Data.Sequence hiding (zipWith, replicate, length)
import Data.Foldable (toList)
--import Prelude hiding (length)

main = do
    contents <- readFile "inputs/day9.txt"
    print $ sum $ zipWith (*) [0..] $ toList $ compact $ fromList $ parse' contents

parse :: String -> [Maybe Int]
parse [x] = replicate (read [x]) (Just 0)
parse (x:y:ys) = replicate (read [x]) (Just 0) ++ replicate (read [y]) Nothing ++ map (fmap (+1)) (parse ys)

parse' :: String -> [Maybe Int]
parse' s = f s 0
    where f [x] n = replicate (read [x]) (Just n)
          f (x:y:ys) n = replicate (read [x]) (Just n) ++ replicate (read [y]) Nothing ++ f ys (n + 1)

compact :: Seq (Maybe Int) -> Seq Int
compact Empty = empty
compact (xs :|> Nothing) = compact xs
compact (Just x :<| xs) = x <| compact xs
compact (Nothing :<| (xs :|> Just x)) = x <| compact xs
