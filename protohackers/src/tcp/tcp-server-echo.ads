package TCP.Server.Echo is

   procedure Start (Port : in Port_Number);
   procedure Stop_After_Next_Request;

private

   Shutdown_Flag : Boolean;

   task Service is
      entry Start (Port : in Port_Number);
   end Service;

end TCP.Server.Echo;