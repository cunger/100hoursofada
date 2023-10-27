# Minesweeper

This is an exercise in engineering the logic of a minesweeper game (without graphical user interface).

## Build and test

Build with:
```
$ alr build
```
⚠️ Make sure to compile with switch `-gnata` to enable pre- and post-condition checks.

The unit tests are implemented as an own project in `tests`, which builds on the minesweeper implementation. 
Run them with:
```
$ cd tests
$ alr run
```

## Requirements

### Board

* `MSW-R01` The width and height of the board are configurable.
* `MSW-R02` The number of mines on a board is configurable.
* `MSW-R03` The number of mines has to be non-negative and smaller than the number of cells in a board.
* `MSW-R04` No cell can hold more than one mine, so when generating a board with a specified number of mines, there are exactly that many cells with a mine.
* `MSW-R05` The placing of mines is random, so repeated board generations with the same configuration yield different mine placings.
* `MSW-R06` Each cell of the board holds a number that corresponds to the number of mines in its neighbourhood, where the neighbourhood is a collection of all adjacent cells (row and/or column +/- 1).
* `MSW-R07` Each cell is initially hidden and unflagged. 
* `MSW-R08` Each unflagged cell can be revealed. A flagged cell cannot be revealed.
* `MSW-R09` Each hidden cell can be flagged and unflagged any number of times.
* `MSW-R10` Once a cell was revealed, its state is fixed, i.e. it cannot be hidden again and it cannot be flagged or unflagged.

| Requirement | Implemented by | Test suite | Tests |
|-------------|----------------|------------|-------|
| `MSW-R01` | | | |
| `MSW-R02` | | | |
| `MSW-R03` | precondition of `Generate_Board` | | |
| `MSW-R04` | | `Board_Generation_Test_Suite` | `Check_Number_Of_Mined_Cells` |
| `MSW-R05` | | `Board_Generation_Test_Suite` | `Mines_Are_Placed_Randomly` |
| `MSW-R06` | | `Board_Generation_Test_Suite` | `Check_Number_Of_Adjacent_Mines` |
| `MSW-R07` | | `Board_Generation_Test_Suite` | `All_Cells_Are_Hidden_And_Unflagged` |
| `MSW-R08` | precondition of `Reveal` | `Board_Actions_Test_Suite` | `Revealing_An_Unflagged_Cell`, `A_Flagged_Cell_Cannot_Be_Revealed` |
| `MSW-R09` | | `Board_Actions_Test_Suite` | `Flagging_And_Unflagging_A_Cell` |
| `MSW-R10` | | `Board_Actions_Test_Suite` | `Revealing_An_Unflagged_Cell` |

### Game rules

* Revealing a mined cell means the player loses.
* The player wins when all mined cells are flagged, and all unmined cells are revealed.

### Game modes

* There are three game modes:
    * Beginner: 9x9 board with 10 mines
    * Intermediate: 16x16 board with 40 mines
    * Expert: 16x30 board with 99 mines