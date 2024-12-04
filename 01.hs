module Main where

import Data.IntMap qualified as M
import Data.List (sort, transpose)

part_1 :: [[Int]] -> Int
part_1 pairs = sum $ zipWith (\x y -> abs (x - y)) xs ys
  where
    [xs, ys] = map sort $ transpose pairs

part_2 :: [[Int]] -> Int
part_2 pairs =
    let
        [xs, ys] = transpose pairs
        freqY = M.fromListWith (+) $ zip ys (repeat 1)
        weights = M.mapWithKey (\k y -> k * y) freqY
     in
        sum $ map (\k -> M.findWithDefault 0 k weights) xs

main :: IO ()
main = do
    contents <- readFile "input"
    let pairs = map (map read . words) (lines contents) :: [[Int]]
    putStrLn $ "Part 1: " ++ show (part_1 pairs)
    putStrLn $ "Part 2: " ++ show (part_2 pairs)

-- https://adventofcode.com/2024/day/1