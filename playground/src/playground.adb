-------------------------------------------------------------------------------
--  This is a playground for learning Ada. Nothing more, nothing less.
-------------------------------------------------------------------------------
with Inspect;
with Vector_Init;
with Game_Of_Life;
with Ada.Text_IO; use Ada.Text_IO;

procedure Playground is
begin
   Inspect;
   Vector_Init;

   Game_Of_Life.Run (Game_Of_Life.Glider_Collision);

   for I in reverse 1 .. 10 loop
      Put_Line (Integer'Image (I));
   end loop;

   Put_Line ("Lift off!");

end Playground;
