package Smoke_Test is

   subtype Port_Number is Natural range 1 .. 65535;

   procedure Start_TCP_Echo_Server_Listening_To (Port : in Port_Number);

private

   task TCP_Echo_Server is
      entry Start (Port : Port_Number);
   end TCP_Echo_Server;

end Smoke_Test;