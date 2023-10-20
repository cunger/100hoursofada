# Minesweeper

This is an exercise in engineering the logic of a minesweeper game (without graphical user interface).

## Build and test

Build with:
```
$ alr build
```

## Requirements

### Board

* `MSW-R01` The width and height of the board are configurable.
* `MSW-R02` The number of mines on a board is configurable.
* `MSW-R03` The number of mines has to be positive and smaller than the number of cells in a board.
* `MSW-R04` No cell can hold more than one mine, so when generating a board with a specified number of mines, there are exactly that many cells with a mine.
* `MSW-R05` The placing of mines is random, so repeated board generations with the same configuration yield different mine placings.
* `MSW-R06` Each cell of the board holds a number that corresponds to the number of mines in its neighbourhood, where the neighbourhood is a collection of all adjacent cells (row and/or column +/- 1).
* `MSW-R07` Each cell is initially hidden and unflagged. 
* `MSW-R08` Each hidden cell can be revealed.
* `MSW-R09` Each hidden cell can can be flagged and unflagged any number of times.
* `MSW-R10` Once a cell was revealed, its state is fixed, i.e. it cannot be hidden again and it cannot be flagged or unflagged.

| Requirement | Implemented by | Tests |
|-------------|----------------|-------|
| `MSW-R01` | | |
| `MSW-R02` | | |
| `MSW-R03` | | |
| `MSW-R04` | | |
| `MSW-R05` | | |
| `MSW-R06` | | |
| `MSW-R07` | | |
| `MSW-R08` | | |
| `MSW-R09` | | |
| `MSW-R10` | | |

### Game modes

* `MSW-R11` There are three game modes:
    * Beginner: 9x9 board with 10 mines
    * Intermediate: 16x16 board with 40 mines
    * Expert: 16x30 board with 99 mines