{-# OPTIONS_GHC -Wno-x-partial #-}

module Main where

import Data.List (elemIndex, elemIndices, nub)
import Data.Map.Strict (Map, size, (!?))
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set, elems, member)
import Data.Set qualified as S

import Control.Parallel.Strategies (parMap, rdeepseq, rpar)

{-
    Convenience types and aliases
-}
data Direction = North | East | South | West deriving (Eq)
type Position = (Int, Int)
type Blocks = Set Position
type Guard = (Position, Direction)
type Visited = Map Position [Direction]
type Boundary = Position
type Path = [Position]

{-
    General Guard Movement
-}
turn :: Direction -> Direction
turn North = East
turn East = South
turn South = West
turn West = North

reface :: Guard -> Guard
reface (pos, dir) = (pos, turn dir)

nextStep :: Guard -> Guard
nextStep ((r, c), North) = ((r - 1, c), North)
nextStep ((r, c), East) = ((r, c + 1), East)
nextStep ((r, c), South) = ((r + 1, c), South)
nextStep ((r, c), West) = ((r, c - 1), West)

isBlocked :: Blocks -> Guard -> Bool
isBlocked blocks guard = pos `member` blocks
  where
    (pos, _) = nextStep guard

moveGuard :: Blocks -> Guard -> Guard
moveGuard blocks guard = if isBlocked blocks guard then reface guard else nextStep guard

{-
    Stop conditions:
        - Leaving the board
        - Entering a cycle (visited position in same direction)
-}
hasLeft :: Boundary -> Guard -> Bool
hasLeft (br, bc) ((r, c), _) = r < 0 || c < 0 || r > br || c > bc

enteredCycle :: Visited -> Guard -> Bool
enteredCycle visited (pos, dir) = case visited !? pos of
    Just dirs -> dir `elem` dirs
    Nothing -> False

done :: Boundary -> Visited -> Guard -> Bool
done boundary visited guard = hasLeft boundary guard || enteredCycle visited guard

{-
    Build the visited map
    - Start: empty visited map and indended guard position
    - done check (off board or cycle)
    - accept intended guard position (update visited map)
    - calculate next intended guard position

    We accumuate the path positions as we go for Part 2.
    - The path is final to first position, and we'll need to discard the first.
    - There will be dups (r,c) where a turn occured, so we'll unique'em.
    - It's not important to keep the order; we're just trying to drop a block on each.
-}
run :: Boundary -> Blocks -> Guard -> (Visited, [Position], Guard)
run boundary blocks guard =
    until
        (\(v, ps, g) -> done boundary v g)
        ( \(v, ps, g@(p, d)) ->
            let
                v' = M.insertWith (++) p [d] v
                ps' = p : ps
                g' = moveGuard blocks g
             in
                v' `seq` ps' `seq` g' `seq` (v', ps', g')
        )
        (M.empty, [], guard)

finalCount :: Visited -> Int
finalCount = size

-- part_1 unused now that intermediate results are needed for part_2
part_1 :: Boundary -> Blocks -> Guard -> Int
part_1 boundary blocks guard = let (v, p, g) = run boundary blocks guard in finalCount v

part_2 :: Boundary -> Blocks -> Guard -> Path -> Int
part_2 boundary blocks guard ps = length $ filter id $ parMap rpar pred $ init $ nub ps
  where
    pred p = do
        let blocks' = S.insert p blocks
        let (_, _, g) = run boundary blocks' guard
        not $ hasLeft boundary g

{-
    YOLO!!!
-}

main :: IO ()
main = do
    (boundary, blocks, guard) <- parseInput <$> getContents
    let (v, p, _) = run boundary blocks guard
    let p2 = part_2 boundary blocks guard p
    putStrLn $ "Part 1: " ++ show (size v)
    putStrLn $ "Part 2: " ++ show p2

-- extra pass is not efficient, but it doesn't have to be.
parseInput :: String -> (Boundary, Blocks, Guard)
parseInput input =
    let rows = lines input
        boundary = (length rows - 1, length (head rows) - 1)
        blocks = concatMap (\(r, cs) -> [(r, c) | c <- cs]) $ zip [0 ..] $ map (elemIndices '#') rows
        -- hack: in my input and test data, I know it's an Up (^)
        carat = head $ mapMaybe convertTuple $ zip [0 ..] $ map (elemIndex '^') rows
     in (boundary, S.fromList blocks, (carat, North))
  where
    convertTuple :: (Int, Maybe Int) -> Maybe (Int, Int)
    convertTuple (a, Just b) = Just (a, b)
    convertTuple _ = Nothing

{-
    Compiled and run on the old i5 laptop:

    real	0m29.372s
    user	0m29.406s
    sys	    0m0.024s

    With a little parallelism (laptop 4 core)
        ghc -threaded solve.hs
        time (cat input | ./solve +RTS -N )

    real	0m13.966s
    user	0m50.559s
    sys	    0m0.237s
-}