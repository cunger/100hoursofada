with AUnit.Assertions; use AUnit.Assertions;

package body Minesweeper.Board.Generation.Tests is

   procedure Generated_Board_Has_Expected_Size is null;

   procedure All_Cells_Are_Initially_Hidden_And_Unflagged is
      W : Width  := 4;
      H : Height := 5;

      B : Board (Width => W, Height => H);
      C : Cell;
   begin
      B := Generate_Board (
         Number_Of_Columns => W,
         Number_Of_Rows    => H,
         Number_Of_Mines   => 1
      );

      for I in 1 .. H loop
         for J in 1 .. W loop
            -- inspect the cell at (I,J)
            C := B (I, J);
            
            Assert (Is_Hidden (C), "Cell is not hidden (but should initially be)");
            Assert (not Is_Flagged (C), "Cell is flagged (but should initially not be)");
         end loop;
      end loop;
   end All_Cells_Are_Initially_Hidden_And_Unflagged;

   procedure Number_Of_Mined_Cells_Is_As_Expected is null;
   procedure There_Is_Max_One_Mine_Per_Cell is null;
   procedure Mines_Are_Placed_Randomly is null;

end Minesweeper.Board.Generation.Tests;