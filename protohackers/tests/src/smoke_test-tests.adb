with AUnit.Test_Caller;
with AUnit.Assertions; use AUnit.Assertions;
with TCP_Client;

package body Smoke_Test.Tests is

   ----------------------------------------------------------------------------
   -- Test suite
   ----------------------------------------------------------------------------

   package Smoke_Test_Caller is new AUnit.Test_Caller (Test);

   function Smoke_Test_Suite return Access_Test_Suite is
      S : constant Access_Test_Suite := New_Suite;
   begin
      S.Add_Test (Smoke_Test_Caller.Create (
         "TCP request to echo server returns the data it received",
         Check_Echo'Access
      ));
      -- add more tests if you want

      return S;
   end Smoke_Test_Suite;

   ----------------------------------------------------------------------------
   -- Implementation of test cases
   ----------------------------------------------------------------------------

   procedure Check_Echo (T : in out Test) is
      Message : constant String := "Ping";
   begin
      TCP_Echo_Server.Start (7000);

      declare
         Response : constant String := TCP_Client.Send (
            Host_Name => "127.0.1.1",
            Port      => 7000,
            Message   => Message);
      begin

         Assert (
            Response = Message,
            "Server did not return '" & Message & "' but '" & Response & "'"
         );
      end;
   end Check_Echo;

end Smoke_Test.Tests;