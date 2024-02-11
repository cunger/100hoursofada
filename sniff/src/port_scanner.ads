package Port_Scanner is

   procedure Sniff (Host_Name : String);
   -- Scan all ports for the given host and log those that are open.

end Port_Scanner;