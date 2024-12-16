{-# OPTIONS_GHC -Wno-x-partial #-}

-- Day 15 -- Warehouse Woes -- https://adventofcode.com/2024/day/15

module Main where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Map qualified as M
import Data.Set qualified as S
import Parse15 (processInput)
import Play (
    BoxState (..),
    Direction,
    Position,
    PositionSet,
    runTicks,
    (+.),
 )

main :: IO ()
main = do
    (gridMap, instructions) <- processInput <$> getContents

    -- Part One
    let env = S.fromList $ gridMap M.! '#'
    let robot = head $ M.findWithDefault [] '@' gridMap
    let boxes = S.fromList $ M.findWithDefault [] 'O' gridMap
    let (boxes', robot') = runReader (runTicks (boxes, robot) instructions) env
    putStrLn $ "Part 1: " ++ show (gpsScore boxes')

    -- Part Two: Expand, retype, and run
    let robot_2 = let (r, c) = robot in (r, 2 * c)
    let blr@(boxesL, boxesR) = ((S.map (\(r, c) -> (r, 2 * c)) boxes), (S.map (\(r, c) -> (r, 2 * c + 1)) boxes))
    let env_2 = (S.map (\(r, c) -> (r, 2 * c)) env) `S.union` (S.map (\(r, c) -> (r, 2 * c + 1)) env)
    let (boxes_2', robot_2') = runReader (runTicks (blr, robot_2) instructions) env_2
    putStrLn $ "Part 1: " ++ show (gpsScore boxes_2')

    return ()

-- PART ONE

type Part1 = PositionSet

instance BoxState Part1 where
    affectedBoxes boxes robot dir = return $ S.fromList $ takeWhile (`S.member` boxes) $ drop 1 $ iterate (+. dir) robot
    adjustBoxes affected dir = S.map (+. dir) affected
    blocked items = do
        wallSet <- ask
        return $ items `S.intersection` wallSet /= S.empty
    (-:) = S.difference
    (+:) = S.union
    gpsScore = sum . S.map (\(r, c) -> r * 100 + c)

-- PART TWO

type Part2 = (PositionSet, PositionSet)

instance BoxState Part2 where
    affectedBoxes (boxesLeft, boxesRight) robot dir = do
        let either = boxesLeft `S.union` boxesRight
        if dir `elem` "<>"
            then do
                let row = S.fromList $ takeWhile (`S.member` either) $ drop 1 $ iterate (+. dir) robot
                return (row `S.intersection` boxesLeft, row `S.intersection` boxesRight)
            else return $ vAffected (boxesLeft, boxesRight) robot dir

    adjustBoxes (afLeft, afRight) dir = (S.map (+. dir) afLeft, S.map (+. dir) afRight)
    blocked (adjLeft, adjRight) = do
        wallSet <- ask
        return $ (adjLeft `S.union` adjRight) `S.intersection` wallSet /= S.empty
    (-:) (a1, b1) (a2, b2) = (a1 S.\\ a2, b1 S.\\ b2)
    (+:) (a1, b1) (a2, b2) = (a1 `S.union` a2, b1 `S.union` b2)
    gpsScore (sLeft, _) = sum $ S.map (\(r, c) -> r * 100 + c) sLeft

-- PART TWO HELPERS

vAffected :: Part2 -> Position -> Direction -> Part2
vAffected (boxesLeft, boxesRight) robot dir =
    let allBoxes = boxesLeft `S.union` boxesRight
        go' :: Position -> (PositionSet, PositionSet)
        go' p =
            let p' = p +. dir
             in if p' `S.notMember` allBoxes
                    then (S.empty, S.empty)
                    else
                        let (pr, pc) = p'
                            (l, r) = if p' `S.member` boxesLeft then ((pr, pc), (pr, pc + 1)) else ((pr, pc - 1), (pr, pc))
                            leftResult = go' l
                            rightResult = go' r
                         in ( S.singleton l `S.union` fst leftResult `S.union` fst rightResult
                            , S.singleton r `S.union` snd leftResult `S.union` snd rightResult
                            )
     in go' robot
