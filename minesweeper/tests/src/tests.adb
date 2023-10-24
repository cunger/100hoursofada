with AUnit.Run;
with AUnit.Reporter.Text;
with Minesweeper_Test_Suite;

procedure Tests is

   procedure Run_Minesweeper_Tests is
      new AUnit.Run.Test_Runner (Minesweeper_Test_Suite.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin

   Run_Minesweeper_Tests (Reporter);

end Tests;