import Text.Parsec
import Control.Applicative (some)

main = do
    contents <- readFile "inputs/day3.txt"
    print (test parser contents)

test p = parse p ""

number :: Parsec String () String
number = some digit

mul :: Parsec String () Int
mul = do
    string "mul("
    a <- number
    string ","
    b <- number
    string ")"
    return (read a * read b)

nextMul :: Parsec String () Int
nextMul = try mul <|> try (anyChar >> nextMul) <|> (some anyChar >> eof >> pure 0)

parser :: Parsec String () Int
parser = sum <$> many nextMul