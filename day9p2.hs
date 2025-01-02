import Data.Sequence hiding (zipWith, replicate)
import Data.Foldable (toList)
import Prelude hiding (drop, tail, take, length, filter)

main = do
    contents <- readFile "inputs/day9.txt"
    print $ value $ shuffle $ fromList $ parse' contents

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

move :: Int -> Seq (Maybe Int) -> Seq (Maybe Int)
move n s =
    let len = length $ filter (== Just n) s
        start = head $ elemIndicesL (Just n) s
        space = findSpace len s
    in case space of
        Nothing -> s
        Just x | x >= start -> s
        Just x -> swap s len x start

findSpace :: Int -> Seq (Maybe Int) -> Maybe Int
findSpace n s
    | length s < n = Nothing
    | all (== Nothing) (take n s) = Just 0
    | otherwise = (+1) <$> findSpace n (tail s)

tail :: Seq a -> Seq a
tail (_ :<| a) = a

swap :: Seq a -> Int -> Int -> Int -> Seq a
swap s len a b =
    let p1 = take a s
        p2 = take len $ drop a s
        p3 = take (b - a - len) $ drop (a + len) s
        p4 = take len $ drop b s
        p5 = drop (b + len) s
    in p1 >< p4 >< p3 >< p2 >< p5

value :: Seq (Maybe Int) -> Int
value = f 0
    where f n Empty = 0
          f n (Just x :<| xs) = x * n + f (n + 1) xs
          f n (Nothing :<| xs) = f (n + 1) xs

shuffle :: Seq (Maybe Int) -> Seq (Maybe Int)
shuffle Empty = Empty
shuffle (xs :|> Nothing) = shuffle xs :|> Nothing
shuffle s@(_ :|> Just x) = shuffle rest >< done
    where new = move x s
          (done, rest) = spanr (== Just x) new