# Warehouse Woes

Part One is pushing boxes around a grid. In Part Two, the width of board and boxes is doubled, with natural complications in pushing boxes up or down when they overlap.

As a Haskell exercise, I spent a lot of extra time to make this an elegant refactor so the Part One logic worked for Part Two as well.

Since we're given a list of instructions to apply, we structure for a `foldl`. The logic is simple:

1. Given the robot's position and intended direction, determine what boxes may be affected.
2. Calculate where those boxes will land if we go ahead.
3. See if the adjusted locations land on a wall. If so, we're blocked; otherwise, update.

Wash, rinse, repeat. In the end, we'll use `gpsScore` as required by the problem.

For Part One, we use `Set (Int,Int)` to represent the box coordinates. For Part Two, we use `(Set (Int,Int), Set (Int,Int))` -- an ordered pair of Position Sets, where the first Set holds coordinates for the left sides and the second for the right sides of a double-wide blocks:


```haskell
type Position = (Int, Int)
type PositionSet = S.Set Position
type Direction = Char

type Env a = Reader PositionSet a

class BoxState a where
    affectedBoxes :: a -> Position -> Direction -> Env a
    adjustBoxes :: a -> Direction -> a
    blocked :: a -> Env Bool
    (-:) :: a -> a -> a
    (+:) :: a -> a -> a
    gpsScore :: a -> Int
```

The `BoxState` function class lets us share the common `tick` logic with the different input types to track the boxes.


```haskell
tick :: (BoxState a) => (a, Position) -> Direction -> Env (a, Position)
tick (boxes, robot) dir = do
    -- robot blocked?
    walls <- ask
    let robot' = robot +. dir
    let robotBlocked = S.member robot' walls

    -- affected blocked?
    affected <- affectedBoxes boxes robot dir
    let adjusted = adjustBoxes affected dir
    isBlocked <- blocked adjusted

    if robotBlocked || isBlocked
        then return (boxes, robot)
        else
            let
                boxes' = boxes -: affected +: adjusted
             in
                return (boxes', robot')
```

We're using a `Reader` monad to carry around the environment. The environment data was more complicated in the earlier implementations (making the complexity tolerable), but now only carries a position set of the wall coordinates.
