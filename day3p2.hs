import Text.Parsec
import Control.Applicative (some)

data Instruction = Do | Dont | Number Int deriving (Show, Eq)

main = do
    contents <- readFile "inputs/day3.txt"
    case test parser contents of
        Right a -> print $ process a
        Left e -> print e

test p = parse p ""

number :: Parsec String () String
number = some digit

mul :: Parsec String () Instruction
mul = do
    string "mul("
    a <- number
    string ","
    b <- number
    string ")"
    return $ Number (read a * read b)

do' = string "do()" >> pure Do

don't = string "don't()" >> pure Dont

function = choice $ map try [mul, do', don't]


next :: Parsec String () Instruction
next = try function <|> try (anyChar >> next) <|> (some anyChar >> eof >> pure Do)

parser :: Parsec String () [Instruction]
parser = many next

process :: [Instruction] -> Int
process [] = 0
process (Number x:xs) = x + process xs
process (Do:xs) = process xs
process (Dont:xs) = process $ tail $ dropWhile (/= Do) xs