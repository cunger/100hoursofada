with GNAT.Sockets; use GNAT.Sockets;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

package body TCP_Client is

   function Send (Host_Name : in String; Port : in Positive; Message : in String)
      return String
   is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;
   begin
      -- Resolve host name.
      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      Address.Port := Port_Type (Port);

      -- Establish connection to host.
      Create_Socket (Socket);
      Connect_Socket (Socket, Address);

      Channel := Stream (Socket, Address);

      -- Send message.
      String'Output (Channel, Message);

      -- Read response and return it.
      return String'Input (Channel);
   exception when E : others =>
         Ada.Text_IO.Put_Line ("⚡ " & Exception_Name (E) & ": " & Exception_Message (E));
         return "";
   end Send;

end TCP_Client;