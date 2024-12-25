{-# OPTIONS_GHC -Wno-x-partial #-}

module GridParse (
    Point,
    Dir (..),
    PointSet,
    (+.),
    hasTurn,
    turnLeft,
    turnRight,
    turnSet,
    listCharCoord,
    mapCharCoord,
    gridSize,
    mapCoordChar,
    areStraightConnected,
    dTaxi,
    straightConnectedPairs,
) where

import Data.Map qualified as M
import Data.Set qualified as S

import Data.Text qualified as T

type Point = (Int, Int)
type PointSet = S.Set Point

-- ingest basics

gridSize :: String -> (Int, Int)
gridSize input = (length (lines input), length (head (lines input)))

indexList :: [a] -> [(Int, a)]
indexList = zip [0 ..]

listCharCoord :: String -> [(Char, (Int, Int))]
listCharCoord input = do
    (r, row) <- indexList (lines input)
    (c, ch) <- indexList row
    return (ch, (r, c))

mapCharCoord :: String -> M.Map Char [(Int, Int)]
mapCharCoord = M.fromListWith (++) . map (\(ch, rc) -> (ch, [rc])) . listCharCoord

mapCoordChar :: String -> M.Map (Int, Int) Char
mapCoordChar = M.fromList . map (\(ch, rc) -> (rc, ch)) . listCharCoord

-- points

data Dir = U | D | L | R deriving (Show, Eq, Ord)

turnLeft :: Dir -> Dir
turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

turnRight :: Dir -> Dir
turnRight = turnLeft . turnLeft . turnLeft

delta :: Dir -> (Int, Int)
delta U = (-1, 0)
delta D = (1, 0)
delta L = (0, -1)
delta R = (0, 1)

(+.) :: Point -> Dir -> Point
(r, c) +. d = let (dr, dc) = delta d in (r + dr, c + dc)

dTaxi :: Point -> Point -> Int
dTaxi (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

-- direction change possibile here

hasTurn :: PointSet -> Point -> Bool
hasTurn tiles p =
    let dirs = [[U, R], [R, D], [D, L], [U, L]]
     in any (\ds -> all (\d -> (p +. d) `S.member` tiles) ds) dirs

turnSet :: PointSet -> PointSet
turnSet tiles = S.filter (hasTurn tiles) tiles

-- find connected corners

areStraightConnected :: PointSet -> Point -> Point -> Bool
areStraightConnected blocks p1@(r1, c1) p2@(r2, c2)
    | r1 == r2 && c1 == c2 = False
    | r1 /= r2 && c1 /= c2 = False
    | r1 == r2 = all (\c -> (r1, c) `S.notMember` blocks) [min c1 c2 .. max c1 c2]
    | c1 == c2 = all (\r -> (r, c1) `S.notMember` blocks) [min r1 r2 .. max r1 r2]
    | otherwise = error "areStraightConnected: unconsidered case"

noPointsBetween :: PointSet -> Point -> Point -> Bool
noPointsBetween allPoints p1@(r1, c1) p2@(r2, c2) =
    let points = [(r, c) | r <- [min r1 r2 .. max r1 r2], c <- [min c1 c2 .. max c1 c2], (r, c) /= p1, (r, c) /= p2]
     in all (`S.notMember` allPoints) points

straightConnectedPairs :: PointSet -> PointSet -> [(Point, Point)]
straightConnectedPairs blocks turns = do
    (p1, p2) <- [(p1, p2) | p1 <- S.toList turns, p2 <- S.toList turns, p1 < p2]
    if areStraightConnected blocks p1 p2 && noPointsBetween turns p1 p2 then return (p1, p2) else []

-- helper utilities

trimWhitespace :: String -> String
trimWhitespace = T.unpack . T.strip . T.pack
