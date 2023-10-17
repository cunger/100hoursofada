package Minesweeper.Board.Generation is

   function Generate_Board (
      Number_Of_Columns : Width;
      Number_Of_Rows : Height;
      Number_Of_Mines : Positive
   ) return Board;
   --  pre: Number_Of_Mines < Number_Of_Columns * Number_Of_Rows

end Minesweeper.Board.Generation;