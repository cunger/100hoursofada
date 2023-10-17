package body Minesweeper.Board.Generation is

   function Generate_Board (
      Number_Of_Columns : Width;
      Number_Of_Rows : Height;
      Number_Of_Mines : Positive
   ) is null; -- TODO

   procedure Place_Mines (B : in out Board; Number_Of_Mines : Positive) is null;
   procedure Calculate_Markers (B : in out Board) is null;

end Minesweeper.Board.Generation;