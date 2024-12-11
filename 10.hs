module Main where

import Data.Char (digitToInt)
import Data.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Vector qualified as V

-- produce a list [(Char, (Row, Col))] from input
import GridParse (listCharCoord)

main :: IO ()
main = do
    input <- getContents
    let pMap = inputToPMap input -- point -> digit
    let vMap = inputToVMap input -- digit -> [point]
    let (graph, vertexToNode, keyToVertex) = mapToGraph pMap
    let nodesByElt = V.fromList $ map (\x -> mapMaybe keyToVertex $ M.findWithDefault [] x vMap) [0 .. 9]

    putStrLn $ "Part 1: " ++ show (length [(z, n) | z <- nodesByElt V.! 0, n <- nodesByElt V.! 9, G.path graph z n])
    putStrLn $ "Part 2: " ++ show (sum $ M.elems $ scoreZeros graph nodesByElt)

inputToPMap :: String -> M.Map (Int, Int) Int
inputToPMap = M.fromList . map (\(ch, (r, c)) -> ((r, c), (digitToInt ch))) . listCharCoord

inputToVMap :: String -> M.Map Int [(Int, Int)]
inputToVMap = M.fromListWith (++) . map (\(ch, p) -> (digitToInt ch, [p])) . listCharCoord

-- edge is (node, key, [key])
mapToGraph :: M.Map (Int, Int) Int -> (G.Graph, G.Vertex -> (Int, (Int, Int), [(Int, Int)]), (Int, Int) -> Maybe G.Vertex)
mapToGraph m = do
    let edges = M.elems $ M.mapWithKey (\p v -> (v, p, nghPlus m p)) m
    let edges' = filter (\(n, _, nghs) -> n == 9 || not (null nghs)) edges
    G.graphFromEdges edges'
  where
    nghPlus :: M.Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
    nghPlus m (r, c) = [(r', c') | (r', c') <- [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)], (r', c') `M.member` m, m M.! (r', c') == m M.! (r, c) + 1]

--
scoreZeros :: G.Graph -> V.Vector [G.Vertex] -> M.Map Int Int
scoreZeros g nodesByElt = foldl' (\acc nineNode -> M.unionWith (+) acc (onesToNine g nodesByElt nineNode)) M.empty (nodesByElt V.! 9)

onesToNine :: G.Graph -> V.Vector [G.Vertex] -> Int -> M.Map Int Int
onesToNine g nodesByElt nineNode = go 0
  where
    go :: Int -> M.Map Int Int
    go 9 = M.singleton nineNode 1
    go n = do
        let current = nodesByElt V.! n
        let next = go $ n + 1
        M.fromList $ [(c, w) | c <- current, let w = sum [next M.! c' | c' <- M.keys next, G.path g c c'], w /= 0]

{-
Laptop, Interpreter:
real    0m2.199s
user    0m2.112s
sys     0m0.094s

Laptop, Compiled:
real    0m0.869s
user    0m0.853s
sys     0m0.026s
-}

-- https://adventofcode.com/2024/day/10
