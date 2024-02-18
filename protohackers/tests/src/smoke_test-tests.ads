with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package Smoke_Test.Tests is

   function Smoke_Test_Suite return Access_Test_Suite;

private

   type Test is new Test_Fixture with null record;

   -- Test cases

   procedure Check_Echo (T : in out Test);

end Smoke_Test.Tests;