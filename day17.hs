import Control.Monad.Writer
import Control.Monad.State
import Data.Bits

type RegisterSet = (Int, Int, Int, Int)

type Instruction = Int -> WriterT [Int] (State RegisterSet) ()

main = do
    contents <- readFile "inputs/day17.txt"
    let l = lines contents
        a = read $ drop 12 $ head l
        b = read $ drop 12 (l !! 1)
        c = read $ drop 12 (l !! 2)

        program = read ("[" ++ drop 9 (l !! 4) ++ "]")
    print $ run program (a, b, c, 0)

run :: [Int] -> RegisterSet -> [Int]
run program initial = evalState (execWriterT $ fetch program) initial

fetch program = do
    (_, _, _, i) <- get 
    (if length program <= i
        then return ()
        else instr (program !! i) (program !! (i + 1)) >> fetch program)

adv :: Instruction
adv combo = do
    (a, b, c, i) <- get
    operand <- getCombo combo
    put (a `shiftR` operand, b, c, i + 2)

bxl :: Instruction
bxl operand = do
    (a, b, c, i) <- get
    put (a, b `xor` operand, c, i + 2)

bst :: Instruction
bst combo = do
    (a, b, c, i) <- get
    operand <- getCombo combo
    put (a, operand `mod` 8, c, i + 2)

jnz :: Instruction
jnz operand = do
    (a, b, c, i) <- get
    case a of
        0 -> modify (fmap (+2))
        _ -> put (a, b, c, operand)

bxc :: Instruction
bxc _ = do
    (a, b, c, i) <- get
    put (a, b `xor` c, c, i + 2)

out :: Instruction
out combo = do
    operand <- getCombo combo
    tell [operand `mod` 8]
    modify (fmap (+ 2))

bdv :: Instruction
bdv combo = do
    (a, b, c, i) <- get
    operand <- getCombo combo
    put (a, a `shiftR` operand, c, i + 2)

cdv :: Instruction
cdv combo = do
    (a, b, c, i) <- get
    operand <- getCombo combo
    put (a, b, a `shiftR` operand, i + 2)

instr :: Int -> Instruction
instr 0 = adv
instr 1 = bxl
instr 2 = bst
instr 3 = jnz
instr 4 = bxc
instr 5 = out
instr 6 = bdv
instr 7 = cdv

getCombo 0 = return 0
getCombo 1 = return 1
getCombo 2 = return 2
getCombo 3 = return 3
getCombo 4 = do
    (a, _, _, _) <- get
    return a
getCombo 5 = do
    (_, b, _, _) <- get
    return b
getCombo 6 = do
    (_, _, c, _) <- get
    return c
getCombo 7 = undefined