with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package Minesweeper.Boards.Generation.Tests is

   function Board_Generation_Test_Suite return Access_Test_Suite;

private

   type Test is new Test_Fixture with null record;

   -- Test cases

   procedure All_Cells_Are_Hidden_And_Unflagged (T : in out Test);
   -- Test for MSW-R07

   procedure Check_Number_Of_Mined_Cells (T : in out Test);
   -- Test for MSW-R04

   procedure Check_Number_Of_Adjacent_Mines (T : in out Test);
   -- Test for MSW-R06

   procedure Mines_Are_Placed_Randomly (T : in out Test);
   -- Test for MSW-R05

end Minesweeper.Boards.Generation.Tests;