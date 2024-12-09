module Pos where

-- Define a new type for pairs of integers
data Pos = Pos Int Int deriving (Show, Eq, Ord)

toPos :: (Int, Int) -> Pos
toPos (r, c) = Pos r c

inBox :: Int -> Int -> Pos -> Bool
inBox r c (Pos a b) = 0 <= a && a <= r && 0 <= b && b <= c

-- Make Pos an instance of the Num type class
instance Num Pos where
    (Pos a b) + (Pos c d) = Pos (a + c) (b + d)
    (Pos a b) - (Pos c d) = Pos (a - c) (b - d)
    (Pos a b) * (Pos c d) = Pos (a * c) (b * d)
    abs (Pos a b) = Pos (abs a) (abs b)
    signum (Pos a b) = Pos (signum a) (signum b)
    fromInteger n = Pos (fromInteger n) (fromInteger n)
    negate (Pos a b) = Pos (negate a) (negate b)

(*.) :: Int -> Pos -> Pos
n *. (Pos a b) = Pos (n * a) (n * b)
