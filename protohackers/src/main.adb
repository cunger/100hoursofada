with TCP.Server.Echo;

procedure Main is
begin

   -- Problem 1: Smoke test
   TCP.Server.Echo.Start (Port => 7000);

end Main;
