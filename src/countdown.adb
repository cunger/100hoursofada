with Ada.Text_IO; use Ada.Text_IO;

package body Countdown is

   ------------------------------------------------------------------------
   --  Prints a simple countdown from the provided start value to lift off.
   --  TODO Check `From >= 1`
   ------------------------------------------------------------------------
   procedure Start (From : Integer := 10) is

      function To_String (I : Integer) return String renames Integer'Image;

   begin
      for I in reverse 1 .. From loop
         Put_Line (To_String (I));
      end loop;

      Put_Line ("Lift off!");
   end Start;
end Countdown;