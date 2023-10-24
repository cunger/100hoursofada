with AUnit.Test_Caller;
with AUnit.Assertions; use AUnit.Assertions;

package body Minesweeper.Board.Generation.Tests is

   ----------------------------------------------------------------------------
   --  Test suite
   ----------------------------------------------------------------------------

   package Board_Generation_Test_Caller is new AUnit.Test_Caller (Test);

   function Board_Generation_Test_Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Board_Generation_Test_Caller.Create (
         "Board generation : size is height * width",
         Generated_Board_Has_Expected_Size'Access
      ));

      S.Add_Test (Board_Generation_Test_Caller.Create (
         "Board generation : cells are initially hidden and unflagged",
         All_Cells_Are_Initially_Hidden_And_Unflagged'Access
      ));

      S.Add_Test (Board_Generation_Test_Caller.Create (
         "Board generation : number of mined cells is correct",
         Check_Number_Of_Mined_Cells'Access
      ));

      S.Add_Test (Board_Generation_Test_Caller.Create (
         "Board generation : mines are placed randomly",
         Mines_Are_Placed_Randomly'Access
      ));

      return S;
   end Board_Generation_Test_Suite;

   ----------------------------------------------------------------------------
   --  Implementation of test cases
   ----------------------------------------------------------------------------

   procedure Generated_Board_Has_Expected_Size (T : in out Test) is
   begin
      Assert (False, "Not implemented yet");
   end Generated_Board_Has_Expected_Size;

   procedure All_Cells_Are_Initially_Hidden_And_Unflagged (T : in out Test) is
      Cols : constant Height := 4;
      Rows : constant Width  := 5;

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

   procedure Check_Number_Of_Mined_Cells (T : in out Test) is
   begin
      Assert (False, "Not implemented yet");
   end Check_Number_Of_Mined_Cells;

   procedure Mines_Are_Placed_Randomly (T : in out Test) is
   begin
      Assert (False, "Not implemented yet");
   end Mines_Are_Placed_Randomly;

end Minesweeper.Board.Generation.Tests;