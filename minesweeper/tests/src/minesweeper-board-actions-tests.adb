with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Assertions; use AUnit.Assertions;

package body Minesweeper.Board.Actions.Tests is

   ----------------------------------------------------------------------------
   --  Test suite
   ----------------------------------------------------------------------------
   package Board_Actions_Test_Caller is new AUnit.Test_Caller (Board_Actions_Test);

   function Board_Actions_Test_Suite return Access_Test_Suite is
      S : Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Board_Actions_Test_Caller.Create (
        "Board actions : a hidden cell can be flagged and unflagged",
        A_Cell_Can_Repeatedly_Be_Flagged_And_Unflagged'Access
      ));

      S.Add_Test (Board_Actions_Test_Caller.Create (
        "Board actions : one a cell is revealed, no more actions are possible",
        Once_A_Cell_Is_Revealed_Nothing_Can_Be_Done_Anymore'Access
      ));

      return S;
   end Board_Actions_Test_Suite;

   ----------------------------------------------------------------------------
   --  Implementation of test cases
   ----------------------------------------------------------------------------

   procedure A_Cell_Can_Repeatedly_Be_Flagged_And_Unflagged (T : in out Board_Actions_Test) is
   begin
      Assert (False, "Not implemented yet");
   end A_Cell_Can_Repeatedly_Be_Flagged_And_Unflagged;

   procedure Once_A_Cell_Is_Revealed_Nothing_Can_Be_Done_Anymore (T : in out Board_Actions_Test) is
   begin
      Assert (False, "Not implemented yet");
   end Once_A_Cell_Is_Revealed_Nothing_Can_Be_Done_Anymore;

end Minesweeper.Board.Actions.Tests;