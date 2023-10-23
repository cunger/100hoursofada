with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package Minesweeper.Board.Generation.Tests is

   type Board_Generation_Test is new Test_Fixture with null record;
   function Board_Generation_Test_Suite return Access_Test_Suite;

   --  Test cases

   procedure Generated_Board_Has_Expected_Size (T : in out Board_Generation_Test);

   procedure All_Cells_Are_Initially_Hidden_And_Unflagged (T : in out Board_Generation_Test);
   --  Test for MSW-R07
   
   procedure Check_Number_Of_Mined_Cells (T : in out Board_Generation_Test);
   procedure Mines_Are_Placed_Randomly (T : in out Board_Generation_Test);

end Minesweeper.Board.Generation.Tests;