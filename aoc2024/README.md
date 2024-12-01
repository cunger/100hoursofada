# Advent of Code 2024

[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2024-coding-for-a-cause)

[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)

Working on the Advent of Code 2024 puzzles.

Main goals: 
* Improve Ada fluency.
* Get familiar with SPARK.

Build and run with:
```
$ alr run
```

Run GNAT prover with:
```
$ alr gnatprove
```

## Structure of implementation

Each day has the following structure:

```
xx
  |_ aoc2024_xx_input.adb
  |_ aoc2024_xx_input.ads
  |_ aoc2024_xx.adb
  |_ aoc2024_xx.ads
  |_ input_xx.txt
```

The `AOC2024_xx_Input` package defines an `Output_Type`, and implements a function that parses the input file and constructs data of the output type:

```ada
function Parse_Input_Data (File_Name : in String) return Output_Type;
```

The `AOC2024_xx` package defines and implements functions `Solve_Part1` and `Solve_Part2`, that return the solutions to that day's puzzle. They start with parsing the input data, and then do whatever computation is needed that day.