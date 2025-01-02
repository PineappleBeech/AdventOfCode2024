import qualified Data.Map as M
import Data.Bits (xor)
import Data.List (sort, intercalate)

data Op = And | Or | Xor deriving Show

main = do
    contents <- lines <$> readFile "inputs/day24.txt"
    let (fixed, variable) = tail <$> span (/= "") contents
        m = M.union (M.fromList $ map readFixed fixed) $ M.fromList $ map ((\(v1, v2, op, res) -> (res, apply op (m M.! v1) (m M.! v2))) . readVariable) variable
        z = sort $ filter (m M.!) $ filter ((== 'z') . head) $ M.keys m
        graph = ["digraph {"] ++ concatMap (toGraph . readVariable) variable ++ ["}"]

    writeFile "inputs/day24graph.txt" $ unlines graph
    putStrLn $ intercalate "," $ sort ["z05", "tst", "sps", "z11", "frt", "z23", "cgh", "pmd"]


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

toGraph :: (String, String, Op, String) -> [String]
toGraph (v1, v2, op, res) = [
        opname ++ " [label = \"" ++ show op ++ "\", shape = box]",
        v1 ++ " -> " ++ opname,
        v2 ++ " -> " ++ opname,
        opname ++ " -> " ++ res
    ]
    where opname = v1 ++ v2 ++ show op

{- swaps z05, tst, sps, z11, frt, z23, cgh, pmd -}

{-
bit carryin carryout either both carryandone
z01 wrs ktr wmq rkd tfd
z02 ktr vmr ssd hjh cdr
z03 vmr

either XOR carryin -> z
y XOR x -> either
either AND carryin -> carryandone
carryandone OR both -> carryout
x AND y -> both



wmq XOR wrs -> z01
y01 XOR x01 -> wmq
wmq AND wrs -> tfd
tfd OR rkd -> ktr
x01 AND y01 -> rkd
x00 AND y00 -> wrs
-}