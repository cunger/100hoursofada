with Ada.Text_IO;
with AOC2024_01;

procedure AOC2024 is

   type Day is new Positive range 1 .. 25;

   procedure Solve_Day (Number : Day);
   procedure Solve_Day (Number : Day) is
      procedure Print (Line : String) renames Ada.Text_IO.Put_Line;
   begin
      Print ("Solving day" & Number'Image);

      case Number is
         when 1 =>
            Print ("Part 1:" & AOC2024_01.Solution_Part1'Image);
            Print ("Part 2:" & AOC2024_01.Solution_Part2'Image);

         when others =>
            Print ("No solution yet for puzzles of day" & Number'Image);
      end case;
   end Solve_Day;

begin

   Solve_Day (1);

end AOC2024;