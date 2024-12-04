import Text.Regex.TDFA ((=~))

data Toggle = Enabled | Disabled

regex :: String
regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

pairsFromLine :: String -> [[Int]]
pairsFromLine "" = []
pairsFromLine s =
    case s =~ regex :: (String, String, String, [String]) of
        -- (before, match, after, [matchgroup])
        (_, _, remainder, p@[a, b]) -> map read p : pairsFromLine remainder
        _ -> []

simpleSum :: String -> Int
simpleSum = sum . map product . pairsFromLine

toggleSum :: String -> Int
toggleSum = sum . map product . go [] Enabled
  where
    go :: [[Int]] -> Toggle -> String -> [[Int]]
    go acc _ "" = acc
    go acc Enabled s =
        let
            (before, _, after) = s =~ "don't\\(\\)" :: (String, String, String)
         in
            go ((pairsFromLine before) ++ acc) Disabled after
    go acc Disabled s =
        let
            (before, _, after) = s =~ "do\\(\\)" :: (String, String, String)
         in
            go acc Enabled after

main :: IO ()
main = do
    input <- getContents
    putStrLn $ "Part 1: " ++ show (simpleSum input)
    putStrLn $ "Part 2: " ++ show (toggleSum input)