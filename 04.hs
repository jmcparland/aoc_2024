{-# OPTIONS_GHC -Wno-x-partial #-}

module Main where

import Data.List (isPrefixOf, tails, transpose)

-- START PART ONE --

targets :: [String]
targets = ["XMAS", "SAMX"]

-- tally one row's XMAS, forward and backward
inString :: String -> Int
inString s = length $ filter id $ isPrefixOf <$> targets <*> tails s

inRows :: [String] -> Int
inRows = sum . map inString

inColumns :: [String] -> Int
inColumns = inRows . transpose

-- put diagonals in columns
skew :: [[a]] -> [[a]]
skew xs = go 0 xs
 where
  go _ [] = []
  go k (x : xs) = drop k x : go (k + 1) xs

-- diagonals for four rows at a time
inDiag :: [String] -> Int
inDiag xs = go xs 0
 where
  go [] acc = acc
  go xs acc = do
    let xs' = take (length (head targets)) xs
    let sk = skew xs'
    go (tail xs) (acc + inColumns sk)

part1 :: [String] -> Int
part1 xs =
  inRows xs
    + inColumns xs
    + inDiag xs -- top left to bottom right
    + inDiag (reverse xs) -- top right to bottom left

-- END PART ONE / START PART TWO --

-- check the top left corner, then all shift left
inBlock :: [String] -> Int
inBlock xs = go 0 xs
 where
  go acc [] = acc
  go acc xs
    | length xs < 3 = acc -- # rows
    | length (head xs) < 3 = acc -- # columns
    | otherwise = go (acc + isX xs) (map (drop 1) xs)
  isX :: [String] -> Int
  isX xs
    | xs !! 1 !! 1 == 'A'
        && ((xs !! 0 !! 0 == 'M' && xs !! 2 !! 2 == 'S') || (xs !! 0 !! 0 == 'S' && xs !! 2 !! 2 == 'M'))
        && ((xs !! 0 !! 2 == 'M' && xs !! 2 !! 0 == 'S') || (xs !! 0 !! 2 == 'S' && xs !! 2 !! 0 == 'M')) =
        1
    | otherwise = 0

-- operate on top three rows, then shift up. repeat & accumulate
part2 :: [String] -> Int
part2 xs = go 0 xs
 where
  go acc [] = acc
  go acc xs = go (acc + inBlock xs) (tail xs)

-- END PART TWO --

main :: IO ()
main = do
  input <- lines <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
