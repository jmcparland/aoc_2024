{-# OPTIONS_GHC -Wno-x-partial #-}

module GridParse2 (listCharCoord, mapCharCoord, gridSize, mapCoordChar) where

import Data.Map qualified as M

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
