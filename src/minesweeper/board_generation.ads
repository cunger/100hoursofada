package Board.Generation is

   function Generate_Empty_Board (Width, Height) return Board;
   
   procedure Place_Mines (Board, Number_Of_Mines);
   procedure Calculate_Markers (Board);

end Board.Generation;