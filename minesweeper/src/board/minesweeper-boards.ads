package Minesweeper.Boards is

   type Cell is private;

   type Board is array (Positive range <>, Positive range <>) of Cell;

   ------------ Cell accessor functions --------------------

   function Is_Flagged (C : Cell) return Boolean;
   function Is_Mined   (C : Cell) return Boolean;
   function Is_Hidden  (C : Cell) return Boolean;

private

   type Cell is record
      Mined   : Boolean;
      Flagged : Boolean;
      Visible : Boolean;

      -- Calculated: the number of mines in neighboring cells
      Number_of_Adjacent_Mines : Integer range 0 .. 8;
   end record;

end Minesweeper.Boards;