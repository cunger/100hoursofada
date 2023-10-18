package body Minesweeper.Board.Generation is

   procedure Place_Mines (B : in out Board; Number_Of_Mines : Positive);
   procedure Calculate_Markers (B : in out Board);

   --  Generates a board with the given width and height.
   --  Then randomly places the given number of mines
   --  and computes for each cell the number of adjacent mines.
   --  The cells are initially hidden and unflagged, so
   --  the result is a board that is ready to be played.
   function Generate_Board (
      Number_Of_Columns : Width;
      Number_Of_Rows    : Height;
      Number_Of_Mines   : Positive
   ) return Board
   is

      B : Board (1 .. Number_Of_Columns, 1 .. Number_Of_Rows);

   begin
      --  Initialize a board with hidden and unflagged cells,
      --  where no mines are placed yet.
      B := (others => (others => (
         Is_Mined   => False,
         Is_Hidden  => True,
         Is_Flagged => False,
         Number_of_Adjacent_Mines => 0
      )));

      Place_Mines (B, Number_Of_Mines);
      Calculate_Markers (B);

      return B;
   end Generate_Board;

   --  Randomly place mines on the board,
   --  by picking cells and marking them as Is_Mined := True.
   procedure Place_Mines (B : in out Board; Number_Of_Mines : Positive) is null;

   --  For each cell, sum the number of adjacent cells that are mined.
   procedure Calculate_Markers (B : in out Board) is null;

end Minesweeper.Board.Generation;