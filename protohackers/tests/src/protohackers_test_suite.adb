with Smoke_Test.Tests; use Smoke_Test.Tests;

package body Protohackers_Test_Suite is

   function Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Smoke_Test_Suite);
      -- tbd: add test suites for the other problems

      return S;
   end Suite;

end Protohackers_Test_Suite;