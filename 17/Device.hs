module Device where

import Data.Bits (Bits (xor), (.<<.))
import Data.Default (Default (def))

runProgram :: Regs -> [Integer] -> Regs
runProgram reg prog =
    until
        (\r -> (ptr r) >= length prog)
        ( \r ->
            let opc = prog !! (ptr r)
                opr = toInteger $ prog !! ((ptr r) + 1)
             in (cmd opc) r opr
        )
        reg

data Regs = Regs
    { aa :: Integer
    , bb :: Integer
    , cc :: Integer
    , ptr :: Int
    , output :: [Integer]
    }
    deriving (Show)

instance Default Regs where
    def = Regs 0 0 0 0 []

combo :: Regs -> Integer -> Integer
combo r 4 = aa r
combo r 5 = bb r
combo r 6 = cc r
combo _ 7 = error "Invalid combo code"
combo _ n = toInteger n

adv :: Regs -> Integer -> Regs
adv reg opr = reg{aa = (aa reg) `div` (1 .<<. (fromInteger $ combo reg opr)), ptr = (ptr reg) + 2}

bxl :: Regs -> Integer -> Regs
bxl reg opr = reg{bb = (bb reg) `xor` opr, ptr = (ptr reg) + 2}

bst :: Regs -> Integer -> Regs
bst reg opr = reg{bb = (combo reg opr) `mod` 8, ptr = (ptr reg) + 2}

jnz :: Regs -> Integer -> Regs
jnz reg opr =
    if (aa reg) /= 0
        then reg{ptr = fromInteger opr}
        else reg{ptr = (ptr reg) + 2}

bxc :: Regs -> Integer -> Regs
bxc reg opr = reg{bb = (bb reg) `xor` (cc reg), ptr = (ptr reg) + 2}

out :: Regs -> Integer -> Regs
out reg opr = reg{output = (output reg) ++ [(combo reg opr) `mod` 8], ptr = (ptr reg) + 2}

bdv :: Regs -> Integer -> Regs
bdv reg opr = reg{bb = (aa reg) `div` (1 .<<. (fromInteger $ combo reg opr)), ptr = (ptr reg) + 2}

cdv :: Regs -> Integer -> Regs
cdv reg opr = reg{cc = (aa reg) `div` (1 .<<. (fromInteger $ combo reg opr)), ptr = (ptr reg) + 2}

cmd :: Integer -> (Regs -> Integer -> Regs)
cmd 0 = adv
cmd 1 = bxl
cmd 2 = bst
cmd 3 = jnz
cmd 4 = bxc
cmd 5 = out
cmd 6 = bdv
cmd 7 = cdv

-- Brief experiment (abandoned)

decompile :: [Integer] -> IO ()
decompile = putStrLn . unlines . go
  where
    go [] = []
    go (opc : opr : rest) = (opcS opc opr) : go rest
    go [opc] = error "Invalid program"

opcS 0 n = "adv" ++ " " ++ opsS n
opcS 1 n = "bxl" ++ " " ++ show n
opcS 2 n = "bst" ++ " " ++ opsS n
opcS 3 n = "jnz" ++ " " ++ show n
opcS 4 n = "bxc"
opcS 5 n = "out" ++ " " ++ opsS n
opcS 6 n = "bdv" ++ " " ++ opsS n
opcS 7 n = "cdv" ++ " " ++ opsS n

opsS 4 = "aa"
opsS 5 = "bb"
opsS 6 = "cc"
opsS 7 = "err"
opsS n = show n
