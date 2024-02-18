with Smoke_Test;

package body Protohackers_Test_Suite is

   function Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Smoke_Test.Smoke_Test_Suite);

      return S;
   end Suite;

end Protohackers_Test_Suite;