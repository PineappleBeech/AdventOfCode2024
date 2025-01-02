import qualified Data.Map as M
import Data.Bits (xor)
import Data.List (sort)

data Op = And | Or | Xor

main = do
    contents <- lines <$> readFile "inputs/day24.txt"
    let (fixed, variable) = tail <$> span (/= "") contents
        m = M.union (M.fromList $ map readFixed fixed) $ M.fromList $ map ((\(v1, v2, op, res) -> (res, apply op (m M.! v1) (m M.! v2))) . readVariable) variable
        z = sort $ filter (m M.!) $ filter ((== 'z') . head) $ M.keys m
    print $ sum $ map value z

readFixed :: String -> (String, Bool)
readFixed [a1, a2, a3, ':', ' ', v] = ([a1, a2, a3], v == '1')

readVariable :: String -> (String, String, Op, String)
readVariable s = (v1, v2, op', result)
    where [v1, op, v2, _, result] = words s
          op' = case op of
            "AND" -> And
            "OR" -> Or
            "XOR" -> Xor

apply op x y = case op of
    And -> x && y
    Or -> x || y
    Xor -> x `xor` y

value :: String -> Int
value (_:s) = 2 ^ read s