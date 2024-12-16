module Play (
    BoxState (..),
    Position,
    PositionSet,
    Direction,
    Env,
    (+.),
    runTicks,
) where

import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader (ask), Reader)
import Data.Set qualified as S

type Position = (Int, Int)
type PositionSet = S.Set Position
type Direction = Char

type Env a = Reader PositionSet a

class BoxState a where
    affectedBoxes :: a -> Position -> Direction -> Env a
    adjustBoxes :: a -> Direction -> a
    blocked :: a -> Env Bool
    (-:) :: a -> a -> a
    (+:) :: a -> a -> a
    gpsScore :: a -> Int

_delta :: Direction -> Position
_delta '^' = (-1, 0)
_delta 'v' = (1, 0)
_delta '<' = (0, -1)
_delta '>' = (0, 1)

(+.) :: Position -> Direction -> Position
(r, c) +. d = (r + dr, c + dc)
  where
    (dr, dc) = _delta d

tick :: (BoxState a) => (a, Position) -> Direction -> Env (a, Position)
tick (boxes, robot) dir = do
    -- robot blocked?
    walls <- ask
    let robot' = robot +. dir
    let robotBlocked = S.member robot' walls

    -- affected blocked?
    affected <- affectedBoxes boxes robot dir
    let adjusted = adjustBoxes affected dir
    isBlocked <- blocked adjusted

    if robotBlocked || isBlocked
        then return (boxes, robot)
        else
            let
                boxes' = boxes -: affected +: adjusted
             in
                return (boxes', robot')

runTicks :: (BoxState a) => (a, Position) -> [Direction] -> Env (a, Position)
runTicks initialState instructions = foldM tick initialState instructions
