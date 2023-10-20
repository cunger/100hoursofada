package Minesweeper.Board is

   subtype Width  is Integer range 1 .. 30;
   subtype Height is Integer range 1 .. 30;

   type Cell is private;

   type Board is array (Width range <>, Height range <>) of Cell;

private

   type Cell is record
      Mined   : Boolean;
      Flagged : Boolean;
      Visible : Boolean;

      --  Calculated: the number of mines in neighboring cells
      Number_of_Adjacent_Mines : Integer range 0 .. 8;
   end record;

end Minesweeper.Board;