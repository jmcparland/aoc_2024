module Devices where

{-
nd=buildPaths numpad 6
map (map ((\s -> s ++ "A") . (map d2c))) $ preImage nd "A029A"

m'=M.map (\dss -> map (map d2c) dss) m

ghci> preImage bp "18"
[[[L,U,L],[U,L,L]],[[R,U,U],[U,R,U],[U,U,R]]]
ghci> map (concat . concatMap ((\pat -> [pat, "A"]) . (map d2c))) $ sequence $ preImage bp "18"
["<^<A>^^A","<^<A^>^A","<^<A^^>A","^<<A>^^A","^<<A^>^A","^<<A^^>A"]

-}

import Data.List (scanl')
import Data.Map qualified as M
import Data.Maybe (mapMaybe)

-- Directional Stuff -- common to both
data Direction = U | D | L | R
    deriving (Show, Eq, Enum, Bounded)

dirs :: [Direction]
dirs = [U, D, L, R]

(+.) :: (Int, Int) -> Direction -> (Int, Int)
(+.) (row, col) U = (row - 1, col)
(+.) (row, col) D = (row + 1, col)
(+.) (row, col) L = (row, col - 1)
(+.) (row, col) R = (row, col + 1)

-- For function reuse in both keypads

type PathsBetweenMap = M.Map (Char, Char) [[Direction]]

data PadData = PadData
    { updateF :: Char -> Direction -> Maybe Char
    , keyChars :: [Char]
    , pathMap :: PathsBetweenMap
    }

instance Show PadData where
    show pad = show $ pathMap pad

-- preimage of sequence
preImage :: PadData -> String -> [[[Direction]]]
preImage pad buttonSeq = do
    let steps = zip ('A' : buttonSeq) buttonSeq
    step <- steps
    return $ (pathMap pad) M.! step

-- Pre-computing Map (start, finish) -> [path]

buildPaths :: PadData -> Int -> PadData
buildPaths pad maxPathLen = pad{pathMap = final}
  where
    final = buildPaths' maxPathLen [[]] M.empty

    buildPaths' :: Int -> [[Direction]] -> PathsBetweenMap -> PathsBetweenMap
    buildPaths' 0 _ m = m
    buildPaths' maxPathLen dss m =
        buildPaths'
            (maxPathLen - 1)
            (extendSeq dss)
            (foldl' (\m' dseq -> (updateMapWithSeq pad) m' dseq) m dss)

extendSeq :: [[Direction]] -> [[Direction]]
extendSeq dss = filter (not . leavesBox) $ (:) <$> dirs <*> dss

leavesBox :: [Direction] -> Bool
leavesBox ds = do
    let walk = scanl' (\p d -> p +. d) (0, 0) ds
    let (row, col) = last walk
    -- stay inside the bounding box between (0,0) and (row,col)
    let (minR, minC) = (min 0 row, min 0 col)
    let (maxR, maxC) = (max 0 row, max 0 col)
    any (\(row', col') -> row' `notElem` [minR .. maxR] || col' `notElem` [minC .. maxC]) walk

updateMapWithSeq :: PadData -> PathsBetweenMap -> [Direction] -> PathsBetweenMap
updateMapWithSeq pad m ds =
    foldl'
        (\m' ((s, f), ds') -> M.insertWith (++) (s, f) ds' m')
        m
        $ mapMaybe (evalSeqForMap pad ds) (keyChars pad)

evalSeqForMap :: PadData -> [Direction] -> Char -> Maybe ((Char, Char), [[Direction]])
evalSeqForMap pad ds start = do
    finish <- foldl' (\c d -> c >>= (\x -> (updateF pad) x d)) (Just start) ds
    return $ ((start, finish), [ds])

-- ACTUALS

{- KEYPAD
    +---+---+---+
    | 7 | 8 | 9 |
    +---+---+---+
    | 4 | 5 | 6 |
    +---+---+---+
    | 1 | 2 | 3 |
    +---+---+---+
        | 0 | A |
        +---+---+
-}

numpad :: PadData
numpad = PadData updateN numpadKeys M.empty

numpadKeys :: [Char]
numpadKeys = "A0123456789"

updateN :: Char -> Direction -> Maybe Char
updateN '7' U = Nothing
updateN '7' D = Just '4'
updateN '7' L = Nothing
updateN '7' R = Just '8'
updateN '8' U = Nothing
updateN '8' D = Just '5'
updateN '8' L = Just '7'
updateN '8' R = Just '9'
updateN '9' U = Nothing
updateN '9' D = Just '6'
updateN '9' L = Just '8'
updateN '9' R = Nothing
updateN '4' U = Just '7'
updateN '4' D = Just '1'
updateN '4' L = Nothing
updateN '4' R = Just '5'
updateN '5' U = Just '8'
updateN '5' D = Just '2'
updateN '5' L = Just '4'
updateN '5' R = Just '6'
updateN '6' U = Just '9'
updateN '6' D = Just '3'
updateN '6' L = Just '5'
updateN '6' R = Nothing
updateN '1' U = Just '4'
updateN '1' D = Nothing
updateN '1' L = Nothing
updateN '1' R = Just '2'
updateN '2' U = Just '5'
updateN '2' D = Just '0'
updateN '2' L = Just '1'
updateN '2' R = Just '3'
updateN '3' U = Just '6'
updateN '3' D = Just 'A'
updateN '3' L = Just '2'
updateN '3' R = Nothing
updateN '0' U = Just '2'
updateN '0' D = Nothing
updateN '0' L = Nothing
updateN '0' R = Just 'A'
updateN 'A' U = Just '3'
updateN 'A' D = Nothing
updateN 'A' L = Just '0'
updateN 'A' R = Nothing
updateN _ _ = error "updateN: invalid input"

{- DIRECTION PAD
        +---+---+
        | ^ | A |
    +---+---+---+
    | < | v | > |
    +---+---+---+
-}

dirpad :: PadData
dirpad = PadData updateK directionKeys M.empty

directionKeys :: [Char]
directionKeys = "A^v<>"

updateK :: Char -> Direction -> Maybe Char
updateK 'A' U = Nothing
updateK 'A' D = Just '>'
updateK 'A' L = Just '^'
updateK 'A' R = Nothing
updateK '^' U = Nothing
updateK '^' D = Just 'v'
updateK '^' L = Nothing
updateK '^' R = Just 'A'
updateK '<' U = Nothing
updateK '<' D = Nothing
updateK '<' L = Nothing
updateK '<' R = Just 'v'
updateK 'v' U = Just '^'
updateK 'v' D = Nothing
updateK 'v' L = Just '<'
updateK 'v' R = Just '>'
updateK '>' U = Just 'A'
updateK '>' D = Nothing
updateK '>' L = Just 'v'
updateK '>' R = Nothing
updateK _ _ = error "updateK: invalid input"

-- TESTING

kGraph :: M.Map Char [Char]
kGraph =
    M.fromList
        [ ('7', "_4_8")
        , ('8', "_579")
        , ('9', "_68_")
        , ('4', "71_5")
        , ('5', "8246")
        , ('6', "935_")
        , ('1', "4__2")
        , ('2', "5013")
        , ('3', "6A2_")
        , ('0', "2__A")
        , ('A', "3_0_")
        ]

-- UDLR

rGraph :: M.Map Char [Char]
rGraph =
    M.fromList
        [ ('^', "_v_A")
        , ('A', "_>^_")
        , ('<', "___v")
        , ('v', "^_<>")
        , ('>', "A_v_")
        ]

c2d :: Char -> Direction
c2d '^' = U
c2d 'v' = D
c2d '<' = L
c2d '>' = R
c2d _ = error "c2d: invalid direction"

d2c :: Direction -> Char
d2c U = '^'
d2c D = 'v'
d2c L = '<'
d2c R = '>'
