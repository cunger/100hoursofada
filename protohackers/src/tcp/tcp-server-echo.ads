-------------------------------------------------------
-- TCP server implementing the echo protocol (RFC 862).
-- https://www.rfc-editor.org/rfc/rfc862.html
--
-- Start an echo server like this:
--
-- TCP.Server.Echo.Start (Port => 7000);
--
-- Every message you then send to the server
-- is simply echoed back as a response.
--
-- You can stop the server by calling:
--
-- TCP.Server.Echo.Stop;
-------------------------------------------------------
package TCP.Server.Echo is

   procedure Start (Port : in Port_Number);
   procedure Stop;

private

   task Service is
      entry Start (Port : in Port_Number);
   end Service;

end TCP.Server.Echo;