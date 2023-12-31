with AUnit.Test_Caller;
with AUnit.Assertions; use AUnit.Assertions;

package body Minesweeper.Boards.Generation.Tests is

   ----------------------------------------------------------------------------
   -- Test suite
   ----------------------------------------------------------------------------

   package Board_Generation_Test_Caller is new AUnit.Test_Caller (Test);

   function Board_Generation_Test_Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Board_Generation_Test_Caller.Create (
         "Board generation : cells are initially hidden and unflagged",
         All_Cells_Are_Hidden_And_Unflagged'Access
      ));

      S.Add_Test (Board_Generation_Test_Caller.Create (
         "Board generation : number of mined cells is correct",
         Check_Number_Of_Mined_Cells'Access
      ));

      S.Add_Test (Board_Generation_Test_Caller.Create (
         "Board generation : number of adjacent mines is calculated correctly",
         Check_Number_Of_Adjacent_Mines'Access
      ));

      S.Add_Test (Board_Generation_Test_Caller.Create (
         "Board generation : mines are placed randomly",
         Mines_Are_Placed_Randomly'Access
      ));

      return S;
   end Board_Generation_Test_Suite;

   ----------------------------------------------------------------------------
   -- Fixture: a generated test board
   ----------------------------------------------------------------------------

   Cols  : constant Positive := 10;
   Rows  : constant Positive := 10;
   Mines : constant Positive := 23;

   B : constant Board (1 .. Cols, 1 .. Rows) :=
      Generate_Board (
         Number_Of_Columns => Cols,
         Number_Of_Rows    => Rows,
         Number_Of_Mines   => Mines
      );

   ----------------------------------------------------------------------------
   -- Implementation of test cases
   ----------------------------------------------------------------------------

   -- Check whether in the initial state of the generated board
   -- all cells are hidden and unflagged.
   procedure All_Cells_Are_Hidden_And_Unflagged (T : in out Test) is
      C : Cell;
   begin
      for I in B'Range (1) loop
         for J in B'Range (2) loop
            C := B (I, J);

            Assert (Is_Hidden (C), "Cell is not hidden (but should initially be)");
            Assert (not Is_Flagged (C), "Cell is flagged (but should initially not be)");
         end loop;
      end loop;
   end All_Cells_Are_Hidden_And_Unflagged;

   -- Check whether the generated board has as many mined cells as specified,
   -- so no mines are placed in the same cell.
   procedure Check_Number_Of_Mined_Cells (T : in out Test) is
      N : Natural;
   begin
      -- Count the number of mined cells on the board.
      N := 0;
      for I in B'Range (1) loop
         for J in B'Range (2) loop
            if Is_Mined (B (I, J))
            then
               N := N + 1;
            end if;
         end loop;
      end loop;

      -- Should be the same as the number of mines we wanted to be placed.
      Assert (N = Mines, "Expected" & Mines'Image & " mined cells, but got" & N'Image);
   end Check_Number_Of_Mined_Cells;

   procedure Check_Number_Of_Adjacent_Mines (T : in out Test) is
   begin
      Assert (False, "Not implemented yet");
   end Check_Number_Of_Adjacent_Mines;

   -- Placing mines on the board is done randomly, so the likelihood of
   -- two generated boards of sufficient size being the same is very low.
   procedure Mines_Are_Placed_Randomly (T : in out Test) is
      Other_Board : constant Board := Generate_Board (
         Number_Of_Columns => Cols,
         Number_Of_Rows    => Rows,
         Number_Of_Mines   => Mines
      );
   begin
      Assert (Other_Board /= B, "Two randonly generated boards should not be the same");
   end Mines_Are_Placed_Randomly;

end Minesweeper.Boards.Generation.Tests;