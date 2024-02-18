with TCP.Server;

procedure Main is
   TCP_Echo_Service : TCP.Server.Echo;
begin

   -- Problem 1: Smoke test
   TCP_Echo_Service.Start (Port => 7000);

end Main;
