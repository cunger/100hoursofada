with Ada.Command_Line;
with Ada.Text_IO;
with NBodySystem;

procedure Main is

   package Real_IO is new Ada.Text_IO.Float_IO (NBodySystem.Real);

   N : constant Integer := Integer'Value (Ada.Command_Line.Argument (1));
   -- Note that there are no checks on the input.

begin

   NBodySystem.Initialize;

   Real_IO.Put (NBodySystem.Energy, 1, 9, 0);
   Ada.Text_IO.New_Line;

   for I in 1 .. N loop
      NBodySystem.Advance (0.01);
   end loop;

   Real_IO.Put (NBodySystem.Energy, 1, 9, 0);
end Main;
