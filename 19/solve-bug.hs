{-# OPTIONS_GHC -Wno-x-partial #-}

-- Day 19 -- Linen Layout -- https://adventofcode.com/2024/day/19

module Main where

import Data.List (isPrefixOf, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Control.Monad (foldM)
import Control.Monad.State (State, evalState, execState, get, modify, put, runState)

import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

{-
    Approach: Recursion with cache.

    The count for a string is the sum of the counts of its children,
    where a child string is the suffix of the parent after removing some prefix.

    If s has no children, counts(s) = 0.
-}

type Cache = Map String Int
type StateCache a = State Cache a

{-
    The initial "towels" are not necessarily "primitives" -- one may be composed of others.
    Seed the state from smallest length to largest. If a towel can be built from smaller towels,
    its score will be the ordinary sum of children plus 1 for iteself.
-}
initialState :: [String] -> Cache
initialState ws =
  let ws' = sortBy (\x y -> compare (length x) (length y)) ws :: [String]
   in foldl (\m w -> M.update (\x -> Just (x + 1)) w $ execState (solCount w) m) M.empty ws'

{-
    The core count function:

    The # of solutions for this string is the sum of the # of solutions for its children,
    where a child is a suffix of the string given some known prefix ("towel").
-}
solCount :: String -> StateCache Int
solCount "" = error "Empty string"
solCount s = do
  cache <- get
  case M.lookup s cache of
    Just count -> return count
    Nothing -> do
      -- a new observation
      let prefixes = M.keys cache
      let sufixes = mapMaybe (removePrefixFrom s) prefixes
      if null sufixes -- it wasn't in the cache and it cannot be reduced
        then do
          modify $ M.insert s 0
          return 0
        else do
          score <- childrenSum sufixes
          modify $ M.insert s score
          return score

{-
    Every child-check updates the cache transparently.
-}
childrenSum :: [String] -> StateCache Int
childrenSum =
  foldM
    ( \total sfx -> do
        score <- solCount sfx -- gets child/suffix score (updating cache if nesc)
        return $ total + score -- update the accumulator
    )
    0 -- initial "total" value (accumulator init)

{-
    Return Just suffix if possible, else Nothing.

    Note: This should not return Just "": the prefixes that we're checking are from
    the cache of previously observed states. We would not call this function if the
    entire string was in the cache.
-}
removePrefixFrom :: String -> String -> Maybe String
removePrefixFrom s pre = do
  if pre `isPrefixOf` s
    then
      if pre /= s then Just $ drop (length pre) s else error "Prefix is the same as string"
    else Nothing

main :: IO ()
main = do
  (ws, ls') <- processInput <$> getContents

  -- ls <- shuffleList ls'
  let ls = ls'

  let cache = initialState ws
  let (p1, finalState) = runState (mapM solCount ls) cache

  -- let maxw = maximum $ map length $ ls
  -- mapM_
  --     ( \w -> do
  --         putStr $ w
  --         putStr $ replicate (maxw - length w) ' '
  --         putStr " : "
  --         putStrLn $ show $ finalState M.! w
  --     )
  --     ls

  print $ length $ filter (> 0) p1
  print $ sum p1

  -- mapM putStrLn (sort $ filter (\x -> (finalState M.! x) > 0) ls)

  return ()

{-
    Just handing the input files.
-}
processInput :: String -> ([String], [String])
processInput s = (wrds, pats)
 where
  ls = lines s
  wrds = map trimWhitespace $ splitOn "," $ head ls
  pats = filter (/= "") $ drop 2 ls

trimWhitespace :: String -> String
trimWhitespace = T.unpack . T.strip . T.pack

{-
    Experiment: given an incorrect answer, does the order of inputs affect
    how the cache is built, potentially changing the answer? (No.)
-}
shuffleList :: [a] -> IO [a]
shuffleList xs = do
  gen <- newStdGen
  return $ shuffle' xs (length xs) gen
