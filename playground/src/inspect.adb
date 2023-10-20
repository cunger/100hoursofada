with Ada.Text_IO;
with System;

procedure Inspect is

   procedure Print (Line : String) renames Ada.Text_IO.Put_Line;

begin

   Print ("Integer size: " & Integer'Image (Integer'Size));
   Print ("Integer MIN: " & Integer'Image (Integer'First));
   Print ("Integer MAX: " & Integer'Image (Integer'Last));

end Inspect;