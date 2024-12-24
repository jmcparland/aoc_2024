{-# OPTIONS_GHC -Wno-x-partial #-}

-- Day 19 -- Linen Layout -- https://adventofcode.com/2024/day/19

module Main where

import Control.Monad.State (State, evalState, get, modify)
import Data.List (isPrefixOf)
import Data.Map qualified as M

import Data.List.Split (splitOn)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

type Cache = M.Map String Int
type StateCache a = State Cache a

solCount :: [String] -> String -> StateCache Int
solCount towels design = solCount' design
 where
  solCount' :: String -> StateCache Int
  solCount' "" = return 1
  solCount' design = do
    cache <- get
    case M.lookup design cache of
      Just count -> return count
      Nothing -> do
        let prefixes = filter (`isPrefixOf` design) towels
        childCounts <- mapM solCount' $ map (flip drop design . length) prefixes
        let total = sum childCounts
        modify $ M.insert design total
        return total

main :: IO ()
main = do
  (towels, designs) <- processInput <$> getContents
  let scores = evalState (mapM (solCount towels) designs) M.empty
  putStr "Part 1: "
  print $ length $ filter (> 0) scores
  putStr "Part 2: "
  print $ sum scores

processInput :: String -> ([String], [String])
processInput s = (wrds, pats)
 where
  ls = lines s
  wrds = map trimWhitespace $ splitOn "," $ head ls
  pats = filter (/= "") $ drop 2 ls

trimWhitespace :: String -> String
trimWhitespace = T.unpack . T.strip . T.pack
