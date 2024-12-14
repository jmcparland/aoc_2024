{-# OPTIONS_GHC -Wno-x-partial #-}

-- Day 14 -- Claw Contraption -- https://adventofcode.com/2024/day/14

module Main where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Ord (comparing)
import Parse14 (Robot, getPoints)

board :: [Int]
board = [101, 103] -- [11, 7]

treeCheckTicks :: Int
treeCheckTicks = 10000

advance :: Int -> Robot -> Robot
advance ticks [p, v] = [p'', v]
 where
  offset = map (* ticks) v
  p' = zipWith (+) p offset
  p'' = zipWith mod p' board

advanceAll :: Int -> [Robot] -> [Robot]
advanceAll ticks = map (advance ticks)

justPositions :: [Robot] -> [[Int]]
justPositions = map head

score :: [Robot] -> Int
score rs = product $ map length [q1, q2, q3, q4]
 where
  [bx, by] = map (`div` 2) board
  jp = justPositions rs
  q1 = filter (\[x, y] -> x > bx && y > by) jp
  q2 = filter (\[x, y] -> x < bx && y > by) jp
  q3 = filter (\[x, y] -> x < bx && y < by) jp
  q4 = filter (\[x, y] -> x > bx && y < by) jp

display :: [Robot] -> IO ()
display rs = do
  let [bc, br] = board
  let mp = M.fromList $ map (\[x, y] -> ((y, x), "*")) $ justPositions rs
  forM_ [0 .. br] $ \r -> do
    forM_ [0 .. bc] $ \c -> do
      putStr $ M.findWithDefault "." (r, c) mp
    putStrLn ""

part_2 :: [Robot] -> IO ()
part_2 robots = do
  let scores = map (\tick -> (tick, score $ advanceAll tick robots)) [0 .. treeCheckTicks]
  let p@(ticks, bestScore) = head $ sortBy (comparing snd) scores
  putStrLn $ "Part 2: " ++ show p
  display $ advanceAll ticks robots

main :: IO ()
main = do
  robots <- getPoints <$> getContents
  putStrLn $ "Part 1: " ++ show (score $ advanceAll 100 robots)
  part_2 robots
