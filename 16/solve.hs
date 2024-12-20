-- Day 16 -- Reindeer Maze -- https://adventofcode.com/2024/day/16
-- ghc -O2 -threaded -rtsopts solve.hs
-- ./solve +RTS -N < input.txt
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main where

-- optimization stuff
import Control.Parallel.Strategies (parMap, rpar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

-- standards
import Data.Heap qualified as H
import Data.List (groupBy)
import Data.Map.Strict qualified as M
import Data.Set qualified as S

-- custom
import Dijkstra (runDijkstra)
import GridParse2 (
    Dir (..),
    Point,
    PointSet,
    dTaxi,
    gridSize,
    mapCharCoord,
    straightConnectedPairs,
    turnLeft,
    turnRight,
    turnSet,
 )

-- convenience
type Score = Int
type Node = (Point, Dir)

main :: IO ()
main = do
    startTime <- getCurrentTime

    -- preprocess - reduce the problem space to "turn points" and "blocks"
    input <- getContents
    let cLocs = mapCharCoord input
    let (gr, gc) = let (gr, gc) = gridSize input in (gr - 1, gc - 1)
    let blocks = S.filter (\(r, c) -> 0 < r && r < gr && 0 < c && c < gc) $ S.fromList (cLocs M.! '#')
    let start = head $ cLocs M.! 'S'
    let finish = head $ cLocs M.! 'E'
    let turns' = turnSet $ S.fromList (cLocs M.! '.')
    let turns = S.union turns' $ S.fromList [start, finish] -- start is actually in a dead-end
    let connected = straightConnectedPairs blocks turns
    let adjMap = M.fromListWith (++) $ concatMap (\(a, b) -> [(a, [b]), (b, [a])]) connected

    let !_ = adjMap `seq` ()
    preprocessTime <- getCurrentTime

    -- Dijkstra
    let initialBest = M.singleton (start, R) 0
    let initialVisited = S.empty
    let (best, preds) = runDijkstra (memoizedNeighbors adjMap) cost initialBest

    let !_ = best `seq` preds `seq` ()
    dijkTime <- getCurrentTime

    -- Part 1
    let m' = M.filterWithKey (\(p, d) s -> p == finish) best
    let p1Ans = minimum m'

    let !_ = p1Ans `seq` ()
    p1Time <- getCurrentTime

    -- Part 2
    let ps = finalPoints preds (finish, U) (start, R)
    let p2Ans = length $ groupBy (\(p, _) (q, _) -> p == q) $ S.toList ps

    let !_ = p2Ans `seq` ()
    p2Time <- getCurrentTime

    endTime <- getCurrentTime

    -- output

    putStrLn "Preprocessing:"
    putStrLn $ "Turns:                   " ++ show (S.size turns)
    putStrLn $ "Interior Blocks:         " ++ show (S.size blocks)
    putStrLn $ "Start:                   " ++ show start
    putStrLn $ "Finish:                  " ++ show finish
    putStrLn $ "Time to preprocess:      " ++ show (diffUTCTime preprocessTime startTime)

    putStrLn $ "Time to run Dijkstra:    " ++ show (diffUTCTime dijkTime preprocessTime)
    putStrLn $ "Time to solve Part 1:    " ++ show (diffUTCTime p1Time dijkTime)
    putStrLn $ "Time to solve Part 2:    " ++ show (diffUTCTime p2Time p1Time)
    putStrLn $ "Total execution time:    " ++ show (diffUTCTime endTime startTime)
    putStrLn ""

    putStrLn "Results:\n"
    putStrLn $ "Part 1: " ++ show p1Ans
    putStrLn $ "Part 2: " ++ show p2Ans

-- Memoization bolt-on wrapper
{-# NOINLINE neighborsCache #-}
neighborsCache :: IORef (M.Map Node [Node])
neighborsCache = unsafePerformIO (newIORef M.empty)

memoizedNeighbors :: M.Map Point [Point] -> Node -> [Node]
memoizedNeighbors adjMap node = unsafePerformIO $ do
    cache <- readIORef neighborsCache
    case M.lookup node cache of
        Just result -> return result
        Nothing -> do
            let result = neighbors adjMap node
            writeIORef neighborsCache (M.insert node result cache)
            return result

neighbors :: M.Map Point [Point] -> Node -> [Node]
neighbors adjMap ((r, c), d) =
    let turned = [((r, c), turnLeft d), ((r, c), turnRight d)]
        eCon = M.findWithDefault [] (r, c) adjMap
        eNghs dir =
            filter
                ( \(r', c') -> case dir of
                    L -> r' == r && c' < c
                    R -> r' == r && c' > c
                    D -> r' > r && c' == c
                    U -> r' < r && c' == c
                )
                eCon
     in turned ++ map (\p -> (p, d)) (eNghs d)

cost :: (Point, Dir) -> (Point, Dir) -> Int
cost (p, _) (q, _) = if p == q then 1000 else dTaxi p q

--

expandLeg :: Node -> Node -> S.Set Node
expandLeg p@((r, c), d) q@((r', c'), d')
    | d == d' =
        let
            rl = [min r r' .. max r r']
            cl = [min c c' .. max c c']
         in
            S.fromList [((y, x), d) | y <- rl, x <- cl]
    | otherwise = S.fromList [p, q]

finalPoints :: M.Map Node (S.Set Node) -> Node -> Node -> S.Set Node
finalPoints rmap finish start = go finish (S.singleton finish) S.empty
  where
    go :: Node -> S.Set Node -> S.Set Node -> S.Set Node
    go p visited acc
        | p == start = acc
        | otherwise =
            let ngs = M.findWithDefault S.empty p rmap
                toVisit = ngs S.\\ visited
                newVisited = S.insert p visited
                -- results = map (\q -> go q newVisited (S.union acc (expandLeg p q))) (S.toList toVisit)
                results = parMap rpar (\q -> go q newVisited (S.union acc (expandLeg p q))) (S.toList toVisit)
             in S.unions results