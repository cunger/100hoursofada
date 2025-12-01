with Ada.Text_IO;
with Day_01;

procedure Aoc2025 is
   procedure Print (Line : String) renames Ada.Text_IO.Put_Line;
begin
   Print ("---- Day 1 ----");
   Day_01.Solve;
end Aoc2025;
