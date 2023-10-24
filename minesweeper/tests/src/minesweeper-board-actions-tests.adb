with AUnit.Test_Caller;
with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Minesweeper.Board.Actions.Tests is

   ----------------------------------------------------------------------------
   --  Test suite
   ----------------------------------------------------------------------------

   package Board_Actions_Test_Caller is new AUnit.Test_Caller (Test);

   function Board_Actions_Test_Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := New_Suite;
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

   function CreateCell return Cell is
   begin
      return (
         Mined   => False,
         Flagged => False,
         Visible => False,
         Number_of_Adjacent_Mines => 0
      );
   end CreateCell;

   procedure Flagging_And_Unflagging_A_Cell (T : in out Test) is
      C : Cell;
   begin
      C := CreateCell;

      Toggle_Flag (C);
      Assert (Is_Flagged (C), "");

      Toggle_Flag (C);
      Assert (not Is_Flagged (C), "");

      Toggle_Flag (C);
      Assert (Is_Flagged (C), "");
   end Flagging_And_Unflagging_A_Cell;

   procedure Revealing_A_Cell (T : in out Test) is
      C : Cell;
   begin
      C := CreateCell;

      Reveal (C);
      Assert (not Is_Hidden (C), "");

      Cannot_Be_Flagged:
      begin
         Toggle_Flag (C);

         Assert (False, "should not have been possible to toggle flag");
      exception
         when System.Assertions.Assert_Failure => Assert (True, "");
      end Cannot_Be_Flagged;
   end Revealing_A_Cell;

end Minesweeper.Board.Actions.Tests;