with AUnit.Test_Caller;
with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Minesweeper.Boards.Actions.Tests is

   ----------------------------------------------------------------------------
   -- Test suite
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
         "Board actions : a flagged cell cannot be revealed",
         A_Flagged_Cell_Cannot_Be_Revealed'Access
      ));

      S.Add_Test (Board_Actions_Test_Caller.Create (
         "Board actions : once a cell is revealed, no more actions are possible",
         Revealing_An_Unflagged_Cell'Access
      ));

      return S;
   end Board_Actions_Test_Suite;

   ----------------------------------------------------------------------------
   -- Implementation of test cases
   ----------------------------------------------------------------------------

   -- Check that a hidden cell can be flagged and unflagged repeatedly.
   procedure Flagging_And_Unflagging_A_Cell (T : in out Test) is
      C : Cell := (
         Mined   => False,
         Flagged => False,
         Visible => False,
         Number_of_Adjacent_Mines => 0
      );
   begin
      Toggle_Flag (C);
      Assert (Is_Flagged (C), "Cell should be flagged");

      Toggle_Flag (C);
      Assert (not Is_Flagged (C), "Cell should not be flagged");

      Toggle_Flag (C);
      Assert (Is_Flagged (C), "Cell should be flagged");

      Toggle_Flag (C);
      Assert (not Is_Flagged (C), "Cell should not be flagged");
   end Flagging_And_Unflagging_A_Cell;

   -- Check that a flagged cell cannot be revealed.
   procedure A_Flagged_Cell_Cannot_Be_Revealed (T : in out Test) is
      C : Cell := (
         Mined   => False,
         Flagged => True,
         Visible => False,
         Number_of_Adjacent_Mines => 0
      );
   begin
      Cannot_Be_Revealed:
      begin
         Reveal (C);

         Assert (False, "should not have been possible to reveal cell");
      exception
         when System.Assertions.Assert_Failure => Assert (True, "");
      end Cannot_Be_Revealed;
   end A_Flagged_Cell_Cannot_Be_Revealed;

   -- Check that an unflagged cell can be revealed,
   -- and once it is, it cannot be flagged anymore.
   procedure Revealing_An_Unflagged_Cell (T : in out Test) is
      C : Cell := (
         Mined   => False,
         Flagged => False,
         Visible => False,
         Number_of_Adjacent_Mines => 0
      );
   begin
      Reveal (C);
      Assert (not Is_Hidden (C), "Cell should have been revealed");

      Cannot_Be_Flagged:
      begin
         Toggle_Flag (C);

         Assert (False, "should not have been possible to toggle flag");
      exception
         when System.Assertions.Assert_Failure => Assert (True, "");
      end Cannot_Be_Flagged;
   end Revealing_An_Unflagged_Cell;

end Minesweeper.Boards.Actions.Tests;