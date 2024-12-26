# Advent of Code 2024

By the end of Christmas, I had 40 stars -- two stars for each of the first 20 days. 

My first stall? Day 19: I had a bug that had me two-over the correct Part One answer. Since the Test data passed, it was unlikely that I was ever going to find the correct answer at random. I found someone else's (beautiful, python) solution in the Day 19 Megathread which resembled my approach. I had to use that script to find the correct answer for my input. In the process, I found my counts didn't mirror the correct ones, so the derrived pass/fails were off too. The python script was using the @cache decorator while in Haskell I was operating in a State monad -- so there was still work to do. In the end, my error was seeding initial state incorrectly, declaring the initial "towels" as seen. I posted my answer once my code was finally correct -- two or three days later.

Once behind, I continued "serially:" (1) I felt no urgency to return to AOC (day job, family, etc), and (2) I did not skip ahead to the current day, try to get all the Part Ones, or try any other skip-ahead. I was looking at Day 21 on Christmas Day, thinking, "Nope! Things to do :-)" No regrets!

All in all, I was happy to do all my work in Haskell -- which I just started learning independently this year on a lark -- and I spent *a lot* of time refactoring and trying to work in some more advanced techniques and abstraction. I learned a lot and I'm pleased with the results.  FWIW!

I may spend a few days before New Years Day seeing if I can make a little more progress -- TBD.


Link: [Advent of Code 2024](https://adventofcode.com/2024)

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
| 19  | [Haskell](19/)   |   2   | [Linen Layout](https://adventofcode.com/2024/day/19)        | Recursive DP with State monad. I had a bug where I was two over the correct answer. Days... |
| 20  | [Haskell](20/)   |   2   | [Race Condition](https://adventofcode.com/2024/day/20)        | Umm... Just counting stuff? :-p  (Dijkstra reuse plus a list comprehension.) |

## Additional Modules

| Name                      | Description                                                                                                                                                     |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [GridParse](GridParse.hs) | Helper functions for dealing with Grid input.                                                                                                                   |
| [Pos](Pos.hs)             | `Pos row col` type with component-wise addition, subtraction, and scalar multiplication. `Pos 3 4 + 2 *. Pos 3 2 == Pos 9 8`. `inBox 5 7 (Pos 10 10) == False`. |
| [Parse13](Parse13.hs)     | Parser for the input of Day 13.                                                                                                                                 |
| [Parse14](Parse14.hs)     | Parser for the input of Day 14.                                                                                                                                 |
| [Dijkstra](16/Dijkstra.hs) | Dijkstra with backtracking -- just in case. |

