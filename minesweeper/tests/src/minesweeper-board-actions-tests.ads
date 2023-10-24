with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package Minesweeper.Board.Actions.Tests is

   function Board_Actions_Test_Suite return Access_Test_Suite;

private

   type Test is new Test_Fixture with null record;

   --  Test cases

   procedure Flagging_And_Unflagging_A_Cell (T : in out Test);
   --  Test for MSW-R09

   procedure Revealing_A_Cell (T : in out Test);
   --  Test for MSW-R10

end Minesweeper.Board.Actions.Tests;