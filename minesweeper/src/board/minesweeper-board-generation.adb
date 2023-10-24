package body Minesweeper.Board.Generation is

   --  Randomly place mines on the board.
   procedure Place_Mines (B : in out Board; Number_Of_Mines : Positive);

   --  For each cell, calculate the number of adjacent mines.
   procedure Mark_Cells_With_Number_Of_Adjacent_Mines (B : in out Board);

   --  Generates a board with the given width and height.
   --  Then randomly places the given number of mines
   --  and computes for each cell the number of adjacent mines.
   --  The cells are initially hidden and unflagged, so
   --  the result is a board that is ready to be played.
   function Generate_Board (
      Number_Of_Columns : Height;
      Number_Of_Rows    : Width;
      Number_Of_Mines   : Positive
   ) return Board is

      B : Board (1 .. Number_Of_Columns, 1 .. Number_Of_Rows);

   begin
      --  Initialize a board with hidden and unflagged cells,
      --  where no mines are placed yet.
      B := (others => (others => (
         Mined   => False,
         Flagged => False,
         Visible => False,
         Number_of_Adjacent_Mines => 0
      )));

      Place_Mines (B, Number_Of_Mines);

      Mark_Cells_With_Number_Of_Adjacent_Mines (B);

      return B;
   end Generate_Board;

   procedure Place_Mines (B : in out Board; Number_Of_Mines : Positive) is
   begin
      --  Randomly pick cells and mark them as Is_Mined := True
      null;
   end Place_Mines;

   procedure Mark_Cells_With_Number_Of_Adjacent_Mines (B : in out Board) is
   begin
      --  For each cell, count the number of adjacent cells that are mined.
      null;
   end Mark_Cells_With_Number_Of_Adjacent_Mines;

end Minesweeper.Board.Generation;