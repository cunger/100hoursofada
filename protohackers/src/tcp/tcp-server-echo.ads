------------------------------------------------------
-- TCP server implemeting the echo protocol (RFC 862):
-- https://www.rfc-editor.org/rfc/rfc862.html
--
-- Use like this:
--
-- TCP.Server.Echo.Start (Port => 7000);
--
-- Every message you then send to the server
-- is simply echoed back as a response.
------------------------------------------------------
package TCP.Server.Echo is

   procedure Start (Port : in Port_Number);
   procedure Stop_After_Next_Request;

private

   Shutdown_Flag : Boolean;

   task Service is
      entry Start (Port : in Port_Number);
   end Service;

end TCP.Server.Echo;