with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with Minesweeper.Board.Generation.Tests; use Minesweeper.Board.Generation.Tests;

package body Minesweeper_Test_Suite is

   package Minesweeper_Test_Caller is new AUnit.Test_Caller (Board_Generation_Test);

   function Suite return Access_Test_Suite is
      S : Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Minesweeper_Test_Caller.Create (
        "Board generation : size is height * width",
        Generated_Board_Has_Expected_Size'Access
      ));

      S.Add_Test (Minesweeper_Test_Caller.Create (
        "Board generation : cells are initially hidden and unflagged",
        All_Cells_Are_Initially_Hidden_And_Unflagged'Access
      ));

      S.Add_Test (Minesweeper_Test_Caller.Create (
        "Board generation : number of mined cells is correct",
        Check_Number_Of_Mined_Cells'Access
      ));
      
      S.Add_Test (Minesweeper_Test_Caller.Create (
        "Board generation : mines are placed randomly",
        Mines_Are_Placed_Randomly'Access
      ));

      return S;
   end Suite;

end Minesweeper_Test_Suite;