with AUnit.Run;
with AUnit.Reporter.Text;
with Protohackers_Test_Suite;

procedure Tests is

   procedure Run_Protohackers_Tests is
      new AUnit.Run.Test_Runner (Protohackers_Test_Suite.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin

   Run_Protohackers_Tests (Reporter);

end Tests;