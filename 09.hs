module Main where

import Data.Char (digitToInt, intToDigit)
import Data.Sequence qualified as Seq

-- DATA PREP

strToDigits :: String -> [Int]
strToDigits = map digitToInt

pairSplit :: [Int] -> [(Int, Int)]
pairSplit [] = []
pairSplit [x] = [(x, 0)]
pairSplit (x : y : xs) = (x, y) : pairSplit xs

wordIndex :: [(Int, Int)] -> [(Int, Maybe Int)]
wordIndex = concat . zipWith (\i (blocks, spaces) -> [(blocks, Just i), (spaces, Nothing)]) [0 ..]

compressedToWords :: String -> [(Int, Maybe Int)]
compressedToWords = wordIndex . pairSplit . strToDigits

-- PART 2

part_2 :: Seq.Seq (Int, Maybe Int) -> (Seq.Seq (Int, Maybe Int))
part_2 s | Seq.null s = Seq.empty
part_2 s = do
    let (ts, hs) = Seq.breakr (\(_, mj) -> mj /= Nothing) s
    let (hs' Seq.:|> p@(m, mm)) = hs
    let idx = Seq.findIndexL (\(n, mn) -> mn == Nothing && n >= m) hs'
    case idx of
        Nothing -> (part_2 hs') Seq.>< (p Seq.<| ts)
        Just i -> do
            let (n, mn) = Seq.index hs' i
            let hs'' = Seq.update i p hs'
            let hs''' = Seq.insertAt (i + 1) (n - m, Nothing) hs''
            part_2 hs''' Seq.>< ((m, Nothing) Seq.<| ts)

-- PART 1

findLeftmostNothing :: Seq.Seq (Int, Maybe Int) -> Maybe Int
findLeftmostNothing =
    Seq.findIndexL
        ( \(_, mj) -> case mj of
            Nothing -> True
            Just _ -> False
        )

findRightmostJust :: Seq.Seq (Int, Maybe Int) -> Maybe Int
findRightmostJust =
    Seq.findIndexR
        ( \(_, mj) -> case mj of
            Just _ -> True
            Nothing -> False
        )

swapStep1 :: Seq.Seq (Int, Maybe Int) -> Maybe (Seq.Seq (Int, Maybe Int))
swapStep1 s = do
    l <- findLeftmostNothing s
    r <- findRightmostJust s
    if l >= r
        then Nothing
        else do
            let (bl, _) = Seq.index s l
            let (br, mj) = Seq.index s r
            if bl <= br
                then do
                    let s' = Seq.update l (bl, mj) s
                    let s'' = Seq.update r (br - bl, mj) s'
                    let s''' = Seq.insertAt (r + 1) (bl, Nothing) s''
                    pure s'''
                else do
                    let s' = Seq.update l (br, mj) s
                    let s'' = Seq.update r (br, Nothing) s'
                    let s''' = Seq.insertAt (l + 1) (bl - br, Nothing) s''
                    pure s'''

part_1 :: Seq.Seq (Int, Maybe Int) -> Seq.Seq (Int, Maybe Int)
part_1 s = go s
  where
    go s = case swapStep1 s of
        Just s' -> go s'
        Nothing -> s

-- SCORING

expand :: Seq.Seq (Int, Maybe Int) -> [Integer]
expand = concatMap (\(n, mj) -> replicate n (maybe 0 fromIntegral mj))

checksum :: Seq.Seq (Int, Maybe Int) -> Integer
checksum = sum . zipWith (*) [0 ..] . expand

-- MAIN

main :: IO ()
main = do
    f <- getContents
    let s = Seq.fromList $ compressedToWords f
    putStrLn $ "Part 1: " ++ show (checksum $ part_1 s)
    putStrLn $ "Part 2: " ++ show (checksum $ part_2 s)

{-
    Part 1:
    real	0m3.674s
    user	0m3.667s
    sys	    0m0.015s

    Parts 1 & 2:
    real	0m5.685s
    user	0m5.599s
    sys	    0m0.030s
-}

-- Debugging
showWord :: (Int, Maybe Int) -> String
showWord (i, mj) = replicate i c
  where
    c = case mj of
        Just j -> intToDigit j
        Nothing -> '.'

wordsToString :: [(Int, Maybe Int)] -> String
wordsToString = concatMap showWord