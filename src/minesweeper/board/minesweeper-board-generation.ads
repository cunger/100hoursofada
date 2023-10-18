package Minesweeper.Board.Generation is

   --  Generates a board that is ready to be played.
   function Generate_Board (
      Number_Of_Columns : Width;
      Number_Of_Rows    : Height;
      Number_Of_Mines   : Positive
   ) return Board;
   --  TODO
   --  pre: Number_Of_Mines < Number_Of_Columns * Number_Of_Rows
   --  post: for all cells Is_Hidden = True
   --  post: there are Number_of_Mines cells with Is_Mined = True

end Minesweeper.Board.Generation;