with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Assertions; use AUnit.Assertions;

package body Minesweeper.Board.Actions.Tests is

   ----------------------------------------------------------------------------
   --  Test suite
   ----------------------------------------------------------------------------

   package Board_Actions_Test_Caller is new AUnit.Test_Caller (Test);

   function Board_Actions_Test_Suite return Access_Test_Suite is
      S : Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Board_Actions_Test_Caller.Create (
        "Board actions : a hidden cell can be flagged and unflagged",
        Flagging_And_Unflagging_A_Cell'Access
      ));

      S.Add_Test (Board_Actions_Test_Caller.Create (
        "Board actions : one a cell is revealed, no more actions are possible",
        Revealing_A_Cell'Access
      ));

      return S;
   end Board_Actions_Test_Suite;

   ----------------------------------------------------------------------------
   --  Implementation of test cases
   ----------------------------------------------------------------------------

   procedure Flagging_And_Unflagging_A_Cell (T : in out Test) is
   begin
      Assert (False, "Not implemented yet");
   end Flagging_And_Unflagging_A_Cell;

   procedure Revealing_A_Cell (T : in out Test) is
   begin
      Assert (False, "Not implemented yet");
   end Revealing_A_Cell;

end Minesweeper.Board.Actions.Tests;