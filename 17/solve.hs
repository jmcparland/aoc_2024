{-# OPTIONS_GHC -Wno-x-partial #-}

-- Day 17 -- Chronospatial Computer -- https://adventofcode.com/2024/day/17

module Main where

import Device (Regs (aa, output), runProgram)
import Private (prodProg, prodReg) -- instead of parsing input file, ...

{-
    Ok. Examining sample data, output increases length on powers of 8.
    Also, only two or three low order digits are affected with increase.
    Represent input as a sequence of coefficients of powers of 8 (low to high).
    Build by tacking on to the left (haskell lists).
    Pad out to the length of the program (from obseration).
    BFS: build candidates up to the length of the program.
    Filter by matching length & check increasing match length.
    From the finals (two in my case), convert to decimal and chose the minimum.
    Done.
-}

-- matches from the right, but haskell works from the left
progrev :: [Integer]
progrev = reverse prodProg

-- wrap the program runner to use the A-register
runA :: [Integer] -> [Integer]
runA xs = reverse $ output $ runProgram prodReg{aa = x} prodProg
  where
    x = foldr (\x acc -> acc * 8 + x) 0 xs

part2 :: Integer
part2 =
    minimum $
        map (foldr (\x acc -> acc * 8 + x) 0) $
            filter ((== progrev) . runA) $
                go [[]]
  where
    -- BFS
    go :: [[Integer]] -> [[Integer]]
    go xs = do
        let expanded = concatMap (\s -> [x : s | x <- [1 .. 7]]) xs
        let xs' =
                filter
                    ( \x ->
                        (length (runA (pad x)) == length progrev)
                            && (length x) - 2 <= (countMatches progrev . runA . pad) x
                    )
                    $ expanded
        if null xs' then xs else go xs'

    -- left-pad stem with 0s to length of program
    pad :: [Integer] -> [Integer]
    pad xs = replicate (length progrev - length xs) 0 ++ xs

countMatches :: (Eq a) => [a] -> [a] -> Int
countMatches [] _ = 0
countMatches _ [] = 0
countMatches (x : xs) (y : ys)
    | x == y = 1 + countMatches xs ys
    | otherwise = 0

main :: IO ()
main = do
    let reg = prodReg
    let prog = prodProg

    putStrLn $ "Part 1: " ++ show (runProgram reg prog)
    putStrLn $ "Part 2: " ++ show part2
