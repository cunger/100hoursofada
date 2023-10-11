with Ada.Text_IO;

package body Countdown is

   ------------------------------------------------------------------------
   --  Prints a simple countdown from the provided start value to lift off.
   --  TODO Check `From >= 1`
   ------------------------------------------------------------------------
   procedure Start (From : Integer := 10) is

      package IO renames Ada.Text_IO;
      procedure Print (Line : String) renames IO.Put_Line;
      function To_String (I : Integer) return String renames Integer'Image;

   begin
      for I in reverse 1 .. From loop
         Print (To_String (I));
      end loop;

      Print ("Lift off!");
   end Start;

end Countdown;