------------------------------------------------------------
-- This package generates game boards of given sizes
-- and with a configurable number of mines. 
------------------------------------------------------------
package Minesweeper.Boards.Generation is

   -- Generates a board with all cells initialized 
   -- so that it is ready to be played.
   function Generate_Board (
      Number_Of_Columns : Positive;
      Number_Of_Rows    : Positive;
      Number_Of_Mines   : Positive
   )
   return Board
   with
      Pre => (Number_Of_Mines < Number_Of_Columns * Number_Of_Rows);

end Minesweeper.Boards.Generation;