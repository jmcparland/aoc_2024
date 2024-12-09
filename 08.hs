module Main where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Map qualified as M
import Data.Set qualified as S

-- simple custom imports
import GridParse (charCoordMap, gridSize)
import Pos (Pos, inBox, toPos, (*.))

type Env a = Reader (Int, Int, M.Map Char [Pos]) a
type MethodType = Pos -> Pos -> Env (S.Set Pos)

main :: IO ()
main = do
  input <- getContents
  -- prep data
  let (rb, cb) = let (y, x) = gridSize input in (y - 1, x - 1) -- corner
  let cpm' = M.delete '.' $ charCoordMap input -- Map Char [(Int, Int)]
  let cpm = M.map (map toPos) cpm' -- Map Char [Pos]
  -- do the work
  runSolves (rb, cb, cpm)

runSolves :: (Int, Int, M.Map Char [Pos]) -> IO ()
runSolves env = do
  let results =
        runReader
          ( do
              result1 <- solve strategy_1
              result2 <- solve strategy_2
              return (result1, result2)
          )
          env
  putStrLn $ "Part 1: " ++ show (fst results)
  putStrLn $ "Part 2: " ++ show (snd results)

solve :: MethodType -> Env Int
solve method = do
  (_, _, cpm) <- ask
  sets <-
    mapM
      ( \ch -> do
          let ps = cpm M.! ch
          setList <- mapM (uncurry method) [(p0, p1) | p0 <- ps, p1 <- ps, p0 < p1]
          return $ S.unions setList
      )
      (M.keys cpm)
  return $ S.size $ S.unions sets

strategy_1 :: Pos -> Pos -> Env (S.Set Pos)
strategy_1 p0 p1 = do
  (rb, cb, _) <- ask
  let delta = p1 - p0
  return $ S.fromList $ filter (inBox rb cb) [p0 - delta, p1 + delta]

strategy_2 :: Pos -> Pos -> Env (S.Set Pos)
strategy_2 p0 p1 = do
  (rb, cb, _) <- ask
  let m = p1 - p0
  let fs = takeWhile (inBox rb cb) [p0 + (i *. m) | i <- [0 ..]]
  let bs = takeWhile (inBox rb cb) [p0 - (i *. m) | i <- [1 ..]]
  return $ S.fromList (fs ++ bs)

-- https://adventofcode.com/2024/day/8
