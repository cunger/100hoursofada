with Ada.Numerics.Discrete_Random;

package body Minesweeper.Boards.Generation is

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
      Number_Of_Columns : Positive;
      Number_Of_Rows    : Positive;
      Number_Of_Mines   : Positive
   ) return Board is

      B : Board (1 .. Number_Of_Rows, 1 .. Number_Of_Columns);

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

   -- Marks randomly picked cells as mined.
   -- Does so by shuffling all coordinates of the board, taking a slice of those
   -- and place mines on the corresponding cells.
   procedure Place_Mines (B : in out Board; Number_Of_Mines : Natural) is
      subtype Row_Range is Positive range B'Range (1);
      subtype Col_Range is Positive range B'Range (2);

      -- Define positions on the board
      type Coordinate is record
         Row : Row_Range;
         Col : Col_Range;
      end record;

      subtype Coordinate_Index is Integer range 1 .. Row_Range'Last * Col_Range'Last;
      type Coordinates is array (Coordinate_Index) of Coordinate;

      -- All positions on the board
      All_Coordinates : Coordinates;

      -- For generating random indices
      package Random_Index is new Ada.Numerics.Discrete_Random (Coordinate_Index);
      Random_Index_Generator : Random_Index.Generator;
   begin
      -- Collect all coordinates on the board in the All_Coordinates array
      All_Rows : for I in Row_Range loop
         All_Cols : for J in Col_Range loop
            All_Coordinates ((I - 1) * Col_Range'Last + J) := (I, J);
         end loop All_Cols;
      end loop All_Rows;

      -- Shuffle the coordinates
      Random_Index.Reset (Random_Index_Generator);

      declare
         Random_I : Positive;
         Temp_Coordinate : Coordinate;
      begin
         Shuffle : for I in All_Coordinates'Range loop
            Random_I := Random_Index.Random (Random_Index_Generator);
            Temp_Coordinate := All_Coordinates (I);
            All_Coordinates (I) := All_Coordinates (Random_I);
            All_Coordinates (Random_I) := Temp_Coordinate;
         end loop Shuffle;
      end;

      -- Set the cells at the first Number_Of_Mines coordinates to Mined := True
      Mine_Cells : for I in 1 .. Number_Of_Mines loop
         B (All_Coordinates (I).Row, All_Coordinates (I).Col).Mined := True;
      end loop Mine_Cells;
   end Place_Mines;

   -- For each cell, count the number of adjacent cells that are mined
   -- and store this number in the cell.
   procedure Mark_Cells_With_Number_Of_Adjacent_Mines (B : in out Board) is
      subtype Row_Range is Positive range B'Range (1);
      subtype Col_Range is Positive range B'Range (2);

      Count : Natural range 0 .. 8;
   begin
      -- For each cell at (I, J):
      for I in Row_Range loop
         for J in Col_Range loop
            -- Start counting the number of adjacent mines.
            Count := 0;

            -- For each adjacent cells at (I_Adjacent, J_Adjacent):
            -- Check that the coordinate is on the board and not equal to (I, J),
            -- and then increase the count if the adjacent cell is mined.
            for I_Adjacent in (I - 1) .. (I + 1) loop
               for J_Adjacent in (J - 1) .. (J + 1) loop
                  if I_Adjacent in Row_Range and then
                     J_Adjacent in Col_Range and then
                     I_Adjacent /= I and then
                     J_Adjacent /= J and then
                     Is_Mined (B (I_Adjacent, J_Adjacent))
                  then
                     Count := Count + 1;
                  end if;
               end loop;
            end loop;

            -- Store the number of adjacent mines on the cell.
            B (I, J).Number_of_Adjacent_Mines := Count;
         end loop;
      end loop;
   end Mark_Cells_With_Number_Of_Adjacent_Mines;

end Minesweeper.Boards.Generation;