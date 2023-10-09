with Ada.Text_IO; use Ada.Text_IO;

procedure OneHundredHoursOfAda is
   --
   -- Prints a simple countdown from 10 to lift off.
   --
   procedure Countdown is
   begin
      for I in reverse 1 .. 10 loop
         Put_Line (Integer'Image (I) & "...");
      end loop;

      Put_Line ("Lift off!");
   end Countdown;

begin

   Countdown;

end OneHundredHoursOfAda;
