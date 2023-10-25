package Minesweeper.Board.Generation is

   -- Generates a board that is ready to be played.
   function Generate_Board (
      Number_Of_Columns : Height;
      Number_Of_Rows    : Width;
      Number_Of_Mines   : Positive
   ) return Board
   with Pre => (
      (Number_Of_Mines > 0) and then
      (Number_Of_Mines < Number_Of_Columns * Number_Of_Rows)
   );

end Minesweeper.Board.Generation;