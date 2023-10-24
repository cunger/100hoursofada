with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package Minesweeper.Board.Generation.Tests is

   function Board_Generation_Test_Suite return Access_Test_Suite;

private

   type Test is new Test_Fixture with null record;

   --  Test cases

   procedure Generated_Board_Has_Expected_Size (T : in out Test);

   procedure All_Cells_Are_Initially_Hidden_And_Unflagged (T : in out Test);
   --  Test for MSW-R07
   
   procedure Check_Number_Of_Mined_Cells (T : in out Test);
   --  Test for MSW-R04

   procedure Mines_Are_Placed_Randomly (T : in out Test);
   --  Test for MSW-R05

end Minesweeper.Board.Generation.Tests;