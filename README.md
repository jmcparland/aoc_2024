# Advent of Code 2024

Link: [Advent of Code](https://adventofcode.com/2024)

| Day | Code             | Stars | Title                                                        | Comment                                                                                                                   |
| --- | ---------------- | :---: | ------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------- |
| 01  | [Haskell](01.hs) |   2   | [Historian Hysteria](https://adventofcode.com/2024/day/1)    | List Ops                                                                                                                  |
| 02  | [Haskell](02.hs) |   2   | [Red-Nosed Reports](https://adventofcode.com/2024/day/2)     | Compliance                                                                                                                |
| 03  | [Haskell](03.hs) |   2   | [Mull It Over](https://adventofcode.com/2024/day/3)          | Regex / Parse                                                                                                             |
| 04  | [Haskell](04.hs) |   2   | [Ceres Search](https://adventofcode.com/2024/day/4)          | Grid Ops                                                                                                                  |
| 05  | [Haskell](05.hs) |   2   | [Print Queue](https://adventofcode.com/2024/day/5)           | Funky Sort                                                                                                                |
| 06  | [Haskell](06.hs) |   2   | [Guard Gallivant](https://adventofcode.com/2024/day/6)       | Grid Walks with Bumpers. Busy i5 4-core laptop: 7m at time of solution -> 30s refined -> 15s with trivial parallelism :-) |
| 07  | [Haskell](07.hs) |   2   | [Bridge Repair](https://adventofcode.com/2024day/7)          | Applicative Ops                                                                                                           |
| 08  | [Haskell](08.hs) |   2   | [Resonant Collinearity](https://adventofcode.com/2024/day/8) | Set Ops. After solving, I did a rewrite with Reader monad.                                                                |
| 09  | [Haskell](09.hs) |   2   | [Disk Fragmenter](https://adventofcode.com/2024/day/9)       | Sequence / Deque Ops.                                                                                                     |

## Additional Modules

| Name                      | Description                                                                                                                                                     |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [GridParse](GridParse.hs) | Helper functions for dealing with Grid input.                                                                                                                   |
| [Pos](Pos.hs)             | `Pos row col` type with component-wise addition, subtraction, and scalar multiplication. `Pos 3 4 + 2 *. Pos 3 2 == Pos 9 8`. `inBox 5 7 (Pos 10 10) == False`. |
