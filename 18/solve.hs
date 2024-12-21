{-# OPTIONS_GHC -Wno-x-partial #-}

-- Day 18 -- RAM Run -- https://adventofcode.com/2024/day/18

module Main where

import Data.List (findIndex, tails)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Set qualified as S

import Dijkstra (runDijkstra)
import GridParse2 (Dir (D, L, R, U), (+.))

main :: IO ()
main = do
    part1
    part2
    return ()

-- data arrivs (x, y). translate to (row, col)
parseInput :: String -> [(Int, Int)]
parseInput = map ((\[col, row] -> (row, col)) . map read . splitOn ",") . lines

-- Dijkstra inputs
cost :: (Int, Int) -> (Int, Int) -> Int
cost p q = 1

neighbors :: S.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbors blocks finish@(fr, fc) p =
    let nghs' = S.fromList $ [p +. U, p +. D, p +. L, p +. R]
        ngs = S.filter (\(r, c) -> 0 <= r && r <= fr && 0 <= c && c <= fc) $ nghs' S.\\ blocks
     in S.toList ngs

-- Execution

part1 :: IO ()
part1 = do
    f <- readFile "input"
    let blocks = take 1024 $ parseInput f
    let start = (0, 0)
    let finish = (70, 70)
    let (dijkstra, _) = runDijkstra (neighbors (S.fromList blocks) finish) cost (M.fromList [(start, 0)])
    putStrLn $ "Part 1: " ++ show (dijkstra M.! finish)

part2 :: IO ()
part2 = do
    f <- readFile "input"
    let blocks = parseInput f
    let rbts = tails $ reverse blocks
    let start = (0, 0)
    let finish = (70, 70)
    let existsPath bs = do
            let (dijkstra, _) = runDijkstra (neighbors (S.fromList bs) finish) cost (M.fromList [(start, 0)])
            case M.lookup finish dijkstra of
                Just _ -> True
                Nothing -> False
    case findIndex existsPath rbts of
        Just i -> do
            let (r, c) = rbts !! (i - 1) !! 0
            putStrLn $ "Part 2: " ++ show (c, r)
        Nothing -> putStrLn "No path found"
