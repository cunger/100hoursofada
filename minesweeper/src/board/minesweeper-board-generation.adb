with Ada.Numerics.Discrete_Random;

package body Minesweeper.Board.Generation is

   -- Randomly place mines on the board.
   procedure Place_Mines (B : in out Board; Number_Of_Mines : Natural);

   -- For each cell, calculate the number of adjacent mines.
   procedure Mark_Cells_With_Number_Of_Adjacent_Mines (B : in out Board);

   -- Generates a board with the given width and height.
   -- Then randomly places the given number of mines
   -- and computes for each cell the number of adjacent mines.
   -- The cells are initially hidden and unflagged, so
   -- the result is a board that is ready to be played.
   function Generate_Board (
      Number_Of_Columns : Height;
      Number_Of_Rows    : Width;
      Number_Of_Mines   : Natural
   ) return Board is

      B : Board (1 .. Number_Of_Columns, 1 .. Number_Of_Rows);

   begin
      -- Initialize a board with hidden and unflagged cells,
      -- where no mines are placed yet.
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

   -- Randomly pick cells and mark them as mined.
   procedure Place_Mines (B : in out Board; Number_Of_Mines : Natural) is

      subtype Height_Range is Height range B'Range (1);
      subtype Width_Range  is Width  range B'Range (2);

      package Random_Height is new Ada.Numerics.Discrete_Random (Height_Range);
      package Random_Width  is new Ada.Numerics.Discrete_Random (Width_Range);

      Height_Generator : Random_Height.Generator;
      Width_Generator  : Random_Width.Generator;

      I : Height;
      J : Width;

      Mines_Left_To_Place : Natural := Number_Of_Mines;
   begin
      while Mines_Left_To_Place > 0 loop
         -- Reset random number generators
         Random_Height.Reset (Height_Generator);
         Random_Width.Reset (Width_Generator);

         -- Pick cell at random coordinates
         I := Random_Height.Random (Height_Generator);
         J := Random_Width.Random (Width_Generator);

         -- If it is not mined yet, then mine it.
         if not Is_Mined (B (I, J)) then
            B (I, J).Mined := True;
            Mines_Left_To_Place := Mines_Left_To_Place - 1;
         end if;
         -- Else try with the next random coordinates.
      end loop;
   end Place_Mines;

   -- For each cell, count the number of adjacent cells that are mined.
   procedure Mark_Cells_With_Number_Of_Adjacent_Mines (B : in out Board) is
   begin
      null;
   end Mark_Cells_With_Number_Of_Adjacent_Mines;

end Minesweeper.Board.Generation;