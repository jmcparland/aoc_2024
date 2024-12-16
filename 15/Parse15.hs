module Parse15 where

import Data.Map.Strict qualified as M

import GridParse2 (mapCharCoord)

processInput :: String -> (M.Map Char [(Int, Int)], String)
processInput input = (gByChar, instructions)
 where
  (gridSect, instructionLines) = break (== "") $ lines input
  gByChar = mapCharCoord $ unlines gridSect
  instructions = concat instructionLines
