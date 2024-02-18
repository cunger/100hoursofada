with AUnit.Run;
with AUnit.Reporter.Text;
with Protohackers_Test_Suite;

procedure Tests is

   procedure Run_Protohackers_Tests is
      new AUnit.Run.Test_Runner (Protohackers_Test_Suite.Suite);
   -- add more test runners if you have more test suites

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin

   Run_Protohackers_Tests (Reporter);
   -- execute other test runners if you have more

end Tests;