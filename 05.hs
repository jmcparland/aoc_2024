module Main where

import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.List (find, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Regex.TDFA ((=~))

type Followers = M.IntMap S.IntSet
type ChangeLine = [Int]

ruleOrder :: Followers -> Int -> Int -> Ordering
ruleOrder m a b
    | a == b = EQ
    | a `S.member` (M.findWithDefault S.empty b m) = GT
    | otherwise = LT

part_1 :: Followers -> [ChangeLine] -> Int
part_1 m = score . filter (isValid m)

part_2 :: Followers -> [ChangeLine] -> Int
part_2 m cls = score $ map (makeValid m) $ filter (not . isValid m) cls

isValid :: Followers -> ChangeLine -> Bool
isValid m cl = cl == sortBy (ruleOrder m) cl

makeValid :: Followers -> ChangeLine -> ChangeLine
makeValid m cl = sortBy (ruleOrder m) cl

score :: [ChangeLine] -> Int
score = sum . mapMaybe centerValue
  where
    centerValue :: ChangeLine -> Maybe Int
    centerValue [] = Nothing
    centerValue cs = do
        (x, y) <- find (uncurry (==)) $ zip cs (reverse cs)
        pure x

parseInput :: String -> (Followers, [ChangeLine])
parseInput s = foldl' parseLine (M.empty, []) ls
  where
    ls = lines s
    parseLine (m, cl) line =
        let
            (_, _, _, rule) = line =~ "([0-9]+)\\|([0-9]+)" :: (String, String, String, [String])
            comma_line = line =~ "," :: Bool
            listFromLine = map read $ splitOn "," line :: [Int]
         in
            if comma_line
                then (m, (listFromLine : cl))
                else case rule of
                    [a, b] -> (m', cl)
                      where
                        a' = read a
                        b' = read b
                        m' = M.insertWith S.union a' (S.singleton b') m
                    _ -> (m, cl)

main :: IO ()
main = do
    input <- getContents
    let (m, cl) = parseInput input
    putStrLn $ "Part 1: " ++ show (part_1 m cl)
    putStrLn $ "Part 2: " ++ show (part_2 m cl)
