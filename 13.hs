-- Claw Contraption https://adventofcode.com/2024/day/13

module Main where

import Parse13 (Case (..), getCases) -- Megaparsec parser

data Solution = Solved (Int, Int) | Colinear | NoSolution
    deriving (Show)

cramersInt :: (RealFrac b) => b -> b -> b -> b -> b -> b -> Solution
cramersInt ax ay bx by px py =
    if determinant == 0
        then
            if px * ay == py * ax && px * by == py * bx
                then Colinear
                else NoSolution
        else
            if u /= fromIntegral (round u) || v /= fromIntegral (round v)
                then NoSolution -- not integer solution
                else Solved (round u, round v)
  where
    determinant = ax * by - bx * ay
    u = (px * by - bx * py) / determinant
    v = (ax * py - px * ay) / determinant

solveCase :: Case -> Solution
solveCase Case{buttonA = (ax, ay), buttonB = (bx, by), prize = (px, py)} =
    cramersInt (fromIntegral ax) (fromIntegral ay) (fromIntegral bx) (fromIntegral by) (fromIntegral px) (fromIntegral py)

cost :: Solution -> Int
cost c = case c of
    Solved (x, y) -> 3 * x + y
    Colinear -> error "Unhandled case: Colinear" -- checked data, no colinear cases
    NoSolution -> 0

adjustCase :: Case -> Case
adjustCase c@Case{buttonA = (ax, ay), buttonB = (bx, by), prize = (px, py)} =
    c{prize = (px + 10000000000000, py + 10000000000000)}

main :: IO ()
main = do
    input <- getContents
    let cases = getCases input
    putStrLn $ "Part 1: " ++ show (sum $ map (cost . solveCase) cases)
    putStrLn $ "Part 2: " ++ show (sum $ map (cost . solveCase . adjustCase) cases)
