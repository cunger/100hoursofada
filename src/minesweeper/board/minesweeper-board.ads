package Minesweeper.Board is

   type Width is range 1 .. 30;
   type Height is range 1 .. 30;

   type Cell is private; 
   type Board is private;
   
private

   type Cell is record
      Number_of_Adjacent_Mines : Integer range 1 .. 8;
      Is_Mined : Boolean := False;
      Is_Hidden : Boolean := True;
      Is_Flagged : Boolean := False;
   end record;

   type Board is array (Width, Height) of Cell;

end Minesweeper.Board;