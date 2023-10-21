with AUnit.Assertions; use AUnit.Assertions;

package body Minesweeper.Board.Generation.Tests is

   procedure Generated_Board_Has_Expected_Size (T : in out Board_Generation_Test) is
   begin
      Assert (False, "Not implemented yet");
   end Generated_Board_Has_Expected_Size;

   procedure All_Cells_Are_Initially_Hidden_And_Unflagged (T : in out Board_Generation_Test) is
      Cols : Height := 4;
      Rows : Width  := 5;

      B : Board (1 .. Cols, 1 .. Rows);
      C : Cell;
   begin
      B := Generate_Board (
         Number_Of_Columns => Cols,
         Number_Of_Rows    => Rows,
         Number_Of_Mines   => 1
      );

      for I in 1 .. Cols loop
         for J in 1 .. Rows loop
            -- inspect the cell at (I,J)
            C := B (I, J);
            
            Assert (Is_Hidden (C), "Cell is not hidden (but should initially be)");
            Assert (not Is_Flagged (C), "Cell is flagged (but should initially not be)");
         end loop;
      end loop;
   end All_Cells_Are_Initially_Hidden_And_Unflagged;

   procedure Check_Number_Of_Mined_Cells (T : in out Board_Generation_Test) is
   begin
      Assert (False, "Not implemented yet");
   end Check_Number_Of_Mined_Cells;

   procedure Mines_Are_Placed_Randomly (T : in out Board_Generation_Test) is
   begin
      Assert (False, "Not implemented yet");
   end Mines_Are_Placed_Randomly;

end Minesweeper.Board.Generation.Tests;