{-# OPTIONS_GHC -Wno-x-partial #-}

module Main where

import Data.List.Split (splitOn)

type TestCase = (Int, [Int])
type Operation = Int -> Int -> Int

opList1 :: [Operation]
opList1 = [(+), (*)]

join :: Operation
join x y = read (show x ++ show y)

opList2 :: [Operation]
opList2 = [(+), (*), join]

allOps :: [Operation] -> TestCase -> Int
allOps ops (n, xs) = if any (== n) rs then n else 0
  where
    rs = foldl' (\acc x -> ops <*> acc <*> [x]) [head xs] (tail xs)

solve :: [Operation] -> [TestCase] -> Int
solve ops = sum . map (allOps ops)

main :: IO ()
main = do
    cases <- parseInput <$> getContents
    putStrLn $ "Part 1: " ++ show (solve opList1 cases)
    putStrLn $ "Part 2: " ++ show (solve opList2 cases)

parseInput :: String -> [TestCase]
parseInput c = do
    tl <- lines c
    let [ns, xss] = splitOn ":" tl
    return (read ns :: Int, map read $ words xss :: [Int])
