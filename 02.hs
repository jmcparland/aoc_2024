module Main where

isSafe_1 :: [Int] -> Bool
isSafe_1 measurements = monotonic && bounded
  where
    monotonic = all (> 0) deltas || all (< 0) deltas
    bounded = all (\x -> abs (x) <= 3) deltas
    deltas = case measurements of
        [] -> []
        (_ : xs) -> zipWith (-) xs measurements

part_1 :: [String] -> Int
part_1 = length . filter isSafe_1 . map (map read . words)

exclude_one :: [Int] -> [[Int]]
exclude_one [] = []
exclude_one (x : xs) = xs : map (x :) (exclude_one xs)

isSafe_2 :: [Int] -> Bool
isSafe_2 measurements = any isSafe_1 (exclude_one measurements)

part_2 :: [String] -> Int
part_2 = length . filter isSafe_2 . map (map read . words)

main :: IO ()
main = do
    reports <- lines <$> getContents
    print $ part_1 reports
    print $ part_2 reports

test =
    [ "7 6 4 2 1"
    , "1 2 7 8 9"
    , "9 7 6 2 1"
    , "1 3 2 4 5"
    , "8 6 4 4 1"
    , "1 3 6 7 9"
    ]