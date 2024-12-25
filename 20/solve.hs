{-# OPTIONS_GHC -Wno-x-partial #-}

-- Day 20 -- Race Condition -- https://adventofcode.com/2024/day/20

module Main where

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V

import Dijkstra (runDijkstra)
import GridParse (Point, PointSet, dTaxi, mapCharCoord)

data Grid = Grid
    { blocks :: [Point]
    , tiles :: [Point]
    , start :: Point
    , finish :: Point
    }
    deriving (Show)

shortcuts_1 :: V.Vector Point -> Int -> M.Map Int [(Int, Int)]
shortcuts_1 path minSavings = do
    let lastIndex = V.length path - 1
    let scs =
            [ (i, j)
            | i <- [0 .. lastIndex - 1]
            , j <- [i .. lastIndex]
            , minSavings < (j - i + 1) - 2
            , dTaxi (path V.! i) (path V.! j) == 2
            ]
    M.fromListWith (++) [(j - i - 2, [(i, j)]) | (i, j) <- scs]

shortcuts_2 :: V.Vector Point -> Int -> M.Map Int [(Int, Int)]
shortcuts_2 path minSavings = do
    let lastIndex = V.length path - 1
    let scs =
            [ (i, j)
            | i <- [0 .. lastIndex - 1]
            , j <- [i .. lastIndex]
            , let dist = dTaxi (path V.! i) (path V.! j)
               in 2 <= dist && dist <= 20 && minSavings < (j - i + 1) - dist
            ]
    M.fromListWith (++) [(j - i - dTaxi (path V.! i) (path V.! j), [(i, j)]) | (i, j) <- scs]

main :: IO ()
main = do
    grid <- processInput <$> getContents
    let (best, preds) = runDijkstra (neighbors (tiles grid)) (cost) (M.singleton (start grid) 0)

    putStrLn $ "Start: " ++ show (start grid)
    putStrLn $ "Finish: " ++ show (finish grid)
    putStrLn $ "Best Path Length: " ++ show (best M.! finish grid)

    let path = resolvePath preds (finish grid)

    let scs = shortcuts_1 path 100
    let pathsBySavings = M.map length scs
    putStrLn $ "Part 1: " ++ show (sum pathsBySavings)

    let scs' = shortcuts_2 path 100
    let pathsBySavings' = M.map length scs'
    putStrLn $ "Part 2: " ++ show (sum pathsBySavings')

    return ()

processInput :: String -> Grid
processInput input =
    Grid
        { blocks = blockCoords
        , tiles = tileCoords
        , start = startCoord
        , finish = finishCoord
        }
  where
    charMap = mapCharCoord input
    blockCoords = M.findWithDefault [] '#' charMap
    tileCoords = startCoord : finishCoord : M.findWithDefault [] '.' charMap
    startCoord = head (M.findWithDefault [] 'S' charMap)
    finishCoord = head (M.findWithDefault [] 'E' charMap)

-- Dijkstra

cost :: Point -> Point -> Int
cost _ _ = 1

neighbors :: [Point] -> Point -> [Point]
neighbors tiles (r, c) =
    filter (`elem` tiles) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

resolvePath :: M.Map Point PointSet -> Point -> V.Vector Point
resolvePath preds finish = V.fromList $ reverse $ go finish
  where
    go point = case M.lookup point preds of
        Nothing -> [point]
        Just ps -> point : go (head $ S.toList ps)
