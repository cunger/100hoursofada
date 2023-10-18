package Minesweeper.Board is

   type Width is range 1 .. 30;
   type Height is range 1 .. 30;

   type Cell is private; 

   type Board is array (Width range <>, Height range <>) of Cell;
   
private

   type Cell is record
      Is_Mined   : Boolean;
      Is_Hidden  : Boolean;
      Is_Flagged : Boolean;

      -- Calculated: the number of mines in neighboring cells
      Number_of_Adjacent_Mines : Integer range 0 .. 8;
   end record;

end Minesweeper.Board;