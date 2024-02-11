with GNAT.Sockets; use GNAT.Sockets;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

package body Port_Scanner is

   procedure Sniff (Host_Name : String) is
      Address : Sock_Addr_Type;
      Socket  : Socket_Type;
   begin
      Ada.Text_IO.Put_Line ("Scan: TCP (crude)");
      Ada.Text_IO.Put_Line ("Host: " & Host_Name);

      -- Resolve host name.
      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);

      -- Try connecting to all ports, to see which ones are open.
      Ada.Text_IO.Put_Line ("Open ports:");

      for Port in Port_Type range 1 .. 65535 loop
         Address.Port := Port;

         -- Ask the operating system for a socket.
         Create_Socket (Socket);

         begin
            -- Try to connect the socket to the given port of the host.
            Connect_Socket (Socket, Address);

            -- If the connection was established successfully, the port is open.
            Ada.Text_IO.Put_Line (Port'Image);
         exception
            when Socket_Error =>
               -- If the Error_Type is Connection_Refused, the port is closed.
               -- All other errors should be logged.
               null;
            when E : others =>
               Ada.Text_IO.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         end;

         -- Release socket again.
         Close_Socket (Socket);
      end loop;
   exception when Host_Error =>
      Ada.Text_IO.Put_Line ("⚡ Cannot resolve host.");
   end Sniff;

end Port_Scanner;