# Advent of Code 2021

Working on the Advent of Code 2021 puzzles.

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
  |_ aoc2021_xx_input.adb
  |_ aoc2021_xx_input.ads
  |_ aoc2021_xx.adb
  |_ aoc2021_xx.ads
```

The `AOC2021_xx_Input` package defines an `Output_Type`, and implements a function that parses the input file and constructs data of the output type:

```ada
function Parse_Input_Data (File_Name : in String) return Output_Type;
```

The `AOC2021_xx` package defines and implements functions `Solve_Part1` and `Solve_Part2`, that return the solutions to that day's puzzle. They start with parsing the input data, and then do whatever computation is needed that day.