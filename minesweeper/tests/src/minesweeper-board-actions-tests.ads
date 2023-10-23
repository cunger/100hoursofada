with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package Minesweeper.Board.Actions.Tests is

   type Board_Actions_Test is new Test_Fixture with null record;
   function Board_Actions_Test_Suite return Access_Test_Suite;

   --  Test cases

   procedure A_Cell_Can_Repeatedly_Be_Flagged_And_Unflagged (T : in out Board_Actions_Test);
   procedure Once_A_Cell_Is_Revealed_Nothing_Can_Be_Done_Anymore (T : in out Board_Actions_Test);

end Minesweeper.Board.Actions.Tests;