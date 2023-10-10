package Countdown is

   procedure Start (From : Integer := 10);
   --  Prints a simple countdown from the provided start value to lift off.
   --  The start value needs to be greater than zero;
   --  otherwise there will be no countdown but an immediate lift off.

end Countdown;