with Ada.Text_IO;
with Day_01;
with Day_02;
with Day_03;

procedure Aoc2025 is
   procedure Print (Line : String) renames Ada.Text_IO.Put_Line;
begin

   --Print ("---- Day 1 ----");
   --Day_01.Solve;

   --Print ("---- Day 2 ----");
   --Day_02.Solve;

   Print ("---- Day 3 ----");
   Day_03.Solve;

end Aoc2025;
