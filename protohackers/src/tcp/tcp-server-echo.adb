with GNAT.Sockets; use GNAT.Sockets;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Text_IO;

package body TCP.Server.Echo is

   procedure Start (Port : in Port_Number) is
   begin
      Service.Start (Port);
   end Start;

   procedure Stop is
   begin
      Ada.Task_Identification.Abort_Task (Service'Identity);
   end Stop;

   task body Service is
      Address : Sock_Addr_Type;
      Server  : Socket_Type;
   begin
      accept Start (Port : Port_Number) do
         Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
         Address.Port := Port_Type (Port);

         Create_Socket (Server, Family_Inet, Socket_Stream);

         Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));

         Bind_Socket (Server, Address);
         Listen_Socket (Server);

         Ada.Text_IO.Put_Line ("TCP Echo Server listening on port " & Address.Port'Image);
      end Start;

      -- Accept incoming connection requests.
      declare
         Connection_Socket  : Socket_Type;
         Connection_Address : Sock_Addr_Type;
      begin
         loop
            Accept_Socket (Server, Connection_Socket, Connection_Address);

            -- Read incoming message and echo it back.
            declare
               Channel : constant Stream_Access := Stream (Connection_Socket);
               Message : constant String := String'Input (Channel);
            begin
               String'Output (Channel, Message);

               -- Finally, close the connection.
               Close_Socket (Connection_Socket);
            end;
         end loop;
      end;

   exception when E : others =>
      Ada.Text_IO.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      Close_Socket (Server);

   end Service;

end TCP.Server.Echo;