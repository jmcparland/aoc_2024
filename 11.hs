module Main where

import Data.Map.Strict qualified as M

process :: Integer -> [Integer]
process n
    | n == 0 = [1]
    | even (length $ show n) = let sn = show n in map read [take (length sn `div` 2) sn, drop (length sn `div` 2) sn]
    | otherwise = [n * (2024 :: Integer)]

stoneCounts :: [Integer] -> Int -> Integer
stoneCounts stones depth = do
    let m = M.fromListWith (+) [(x, 1) | x <- stones] in go m depth
  where
    go :: M.Map Integer Integer -> Int -> Integer
    go m 0 = sum $ M.elems m
    go m depth = do
        let ks = M.keys m
        let additions = concatMap (\k -> map (\v -> (v, M.findWithDefault 1 k m)) $ process k) ks
        let subtractions = map (\k -> (k, negate $ M.findWithDefault (-1) k m)) $ M.keys m
        let changedMap = M.fromListWith (+) $ additions ++ subtractions
        let m' = M.filter (> 0) $ M.unionWith (+) m changedMap
        go m' (depth - 1)

main :: IO ()
main = do
    input <- getContents
    let stones = map read $ words input :: [Integer]
    putStrLn $ "Part 1: " ++ show (stoneCounts stones 25)
    putStrLn $ "Part 2: " ++ show (stoneCounts stones 75)

-- https://adventofcode.com/2024/day/11
-- Plutonian Pebbles
