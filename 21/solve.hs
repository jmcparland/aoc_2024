{-# OPTIONS_GHC -Wno-x-partial #-}

-- Day 21 -- Keypad Conundrum -- https://adventofcode.com/2024/day/21

module Main where

import Devices (PadData (..), buildPaths, d2c, dirpad, numpad, preImage)

iterf :: PadData -> String -> [[Char]]
iterf pad =
    map (concat . concatMap ((\pat -> [pat, "A"]) . (map d2c)))
        . sequence
        . preImage pad

minlen :: [[a]] -> Int
minlen = minimum . map length

cull :: [[a]] -> [[a]]
cull ls = filter ((== minlen ls) . length) ls

np :: PadData
np = buildPaths numpad 6

dp :: PadData
dp = buildPaths dirpad 4

p1 :: String -> Int
p1 s = n * ml
  where
    n = read $ init s :: Int

    s1 = cull $ iterf np s
    s2 = cull $ concatMap (iterf dp) s1
    s3 = concatMap (iterf dp) s2
    ml = minlen s3

main :: IO ()
main = do
    -- inputs <- lines <$> readFile "test"
    inputs <- lines <$> readFile "input"
    print $ sum $ map p1 inputs
