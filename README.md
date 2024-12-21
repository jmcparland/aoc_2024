# Advent of Code 2024

Link: [Advent of Code](https://adventofcode.com/2024)

| Day | Code             | Stars | Title                                                        | Comment                                                                                                                        |
| --- | ---------------- | :---: | ------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------ |
| 01  | [Haskell](01.hs) |   2   | [Historian Hysteria](https://adventofcode.com/2024/day/1)    | List Ops                                                                                                                       |
| 02  | [Haskell](02.hs) |   2   | [Red-Nosed Reports](https://adventofcode.com/2024/day/2)     | Compliance                                                                                                                     |
| 03  | [Haskell](03.hs) |   2   | [Mull It Over](https://adventofcode.com/2024/day/3)          | Regex / Parse                                                                                                                  |
| 04  | [Haskell](04.hs) |   2   | [Ceres Search](https://adventofcode.com/2024/day/4)          | Grid Ops                                                                                                                       |
| 05  | [Haskell](05.hs) |   2   | [Print Queue](https://adventofcode.com/2024/day/5)           | Funky Sort                                                                                                                     |
| 06  | [Haskell](06.hs) |   2   | [Guard Gallivant](https://adventofcode.com/2024/day/6)       | Grid Walks with Bumpers. Busy i5 4-core laptop: 7m at time of solution -> 30s refined -> 15s with trivial parallelism :-)      |
| 07  | [Haskell](07.hs) |   2   | [Bridge Repair](https://adventofcode.com/2024day/7)          | Applicative Ops                                                                                                                |
| 08  | [Haskell](08.hs) |   2   | [Resonant Collinearity](https://adventofcode.com/2024/day/8) | Set Ops. After solving, I did a rewrite with Reader monad.                                                                     |
| 09  | [Haskell](09.hs) |   2   | [Disk Fragmenter](https://adventofcode.com/2024/day/9)       | Sequence / Deque Ops.                                                                                                          |
| 10  | [Haskell](10.hs) |   2   | [Hoof It](https://adventofcode.com/2024/day/10)              | Paths & Unique Paths. Some Graph Ops plus one wicked Recursion through Bipartite Graphs. (~2sec interpreter, < 1sec compiled.) |
| 11  | [Haskell](11.hs) |   2   | [Plutonian Pebbles](https://adventofcode.com/2024/day/11)    | Count the Right Things. (0.8 sec in the interpreter)                                                                           |
| 12  | [Haskell](12.hs) |   2   | [Garden Groups](https://adventofcode.com/2024/day/12)        | A Complex Mess :-p Connected components partitioning grid.                                                                     |
| 13  | [Haskell](13.hs) |   2   | [Claw Contraption](https://adventofcode.com/2024/day/13)     | Trivial 2x2 Matrix Solver. More tedius parsing inputs (using separate [Parse13](Parse13.hs).)                                  |
| 14  | [Haskell](14.hs) |   2   | [Restroom Redoubt](https://adventofcode.com/2024/day/14)     | Guards searching for Christmas Tree. Parsing inputs using [Parse14](Parse14.hs).)                                              |
| 15  | [Haskell](15/)   |   2   | [Warehouse Woes](https://adventofcode.com/2024/day/15)       | Pushing Blocks. Beautiful Refactor!                                                                                            |
| 16  | [Haskell](16/)   |   2   | [Reindeer Maze](https://adventofcode.com/2024/day/16)        | Dijkstra with backtracking.                                                                                                    |
| 17  | [Haskell](17/)   |   2   | [Chronospatial Computer](https://adventofcode.com/2024/day/17)        | Manual inspection and experimentation.   |
| 18  | [Haskell](18/)   |   2   | [RAM Run](https://adventofcode.com/2024/day/18)        | Dijkstra reuse. |

## Additional Modules

| Name                      | Description                                                                                                                                                     |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [GridParse](GridParse.hs) | Helper functions for dealing with Grid input.                                                                                                                   |
| [Pos](Pos.hs)             | `Pos row col` type with component-wise addition, subtraction, and scalar multiplication. `Pos 3 4 + 2 *. Pos 3 2 == Pos 9 8`. `inBox 5 7 (Pos 10 10) == False`. |
| [Parse13](Parse13.hs)     | Parser for the input of Day 13.                                                                                                                                 |
| [Parse14](Parse14.hs)     | Parser for the input of Day 14.                                                                                                                                 |
| [Dijkstra](16/Dijkstra.hs) | Dijkstra with backtracking -- just in case. |

