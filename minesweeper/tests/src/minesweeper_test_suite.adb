with Minesweeper.Board.Generation.Tests; use Minesweeper.Board.Generation.Tests;
with Minesweeper.Board.Actions.Tests; use Minesweeper.Board.Actions.Tests;

package body Minesweeper_Test_Suite is

   function Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Board_Generation_Test_Suite);
      S.Add_Test (Board_Actions_Test_Suite);

      return S;
   end Suite;

end Minesweeper_Test_Suite;