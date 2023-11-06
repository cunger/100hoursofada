------------------------------------------------------------
-- This package defines the game board for Minesweeper.
------------------------------------------------------------
package Minesweeper.Boards is

   type Board is private;
   -- A board is a two-dimensional grid consisting of cells.
   -- It's defined by its width (number of columns) and
   -- height (number of rows), as well as the number of
   -- mines distributed on the board.

   type Cell is private;
   -- A cell holds an internal state that partially depends
   -- on its position on the game board and can be changed
   -- through user actions.
   -- This state comprises whether a cell is
   -- * mined (i.e. contains a mine, which will explode when the cell is stepped on)
   -- * flagged (i.e. was marked by the user as containing a mine)
   -- * hidden (i.e. was not yet stepped on to reveal it)
   -- together with the computed number of adjacent cells holding a mine.

   ------------ Cell accessor functions --------------------

   function Is_Flagged (C : Cell) return Boolean;
   function Is_Mined   (C : Cell) return Boolean;
   function Is_Hidden  (C : Cell) return Boolean;

private

   type Board is array (Positive range <>, Positive range <>) of Cell;

   type Cell is record
      Mined   : Boolean;
      Flagged : Boolean;
      Visible : Boolean;

      Number_of_Adjacent_Mines : Integer range 0 .. 8;
      -- The number of mines in neighboring cells
      -- (computed based on the cell's position on a board)
   end record;

end Minesweeper.Boards;