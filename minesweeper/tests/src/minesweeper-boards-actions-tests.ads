with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package Minesweeper.Boards.Actions.Tests is

   function Board_Actions_Test_Suite return Access_Test_Suite;

private

   type Test is new Test_Fixture with null record;

   -- Test cases

   procedure Flagging_And_Unflagging_A_Cell (T : in out Test);
   -- Test for MSW-R09

   procedure A_Flagged_Cell_Cannot_Be_Revealed (T : in out Test);
   -- Test for MSW-R10

   procedure Revealing_An_Unflagged_Cell (T : in out Test);
   -- Test for MSW-R11

end Minesweeper.Boards.Actions.Tests;