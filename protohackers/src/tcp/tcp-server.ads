package TCP.Server is

   -- TCP Echo Server
   task type Echo is

      entry Start (Port : in Port_Number);

   end Echo;

end TCP.Server;