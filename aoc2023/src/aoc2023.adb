with Util.Log.Loggers;
with AOC2023_01;
with AOC2023_02;

procedure AOC2023 is

   type Day is new Positive range 1 .. 25;

   procedure Solve_Day (Number : Day);
   procedure Solve_Day (Number : Day) is
      Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("aoc2023");
   begin
      Log.Info ("Solving day " & Number'Image);

      case Number is
         when 1 =>
            Log.Info ("Part 1: " & AOC2023_01.Sum_Of_Calibration_Values (False)'Image);
            Log.Info ("Part 2: " & AOC2023_01.Sum_Of_Calibration_Values (True)'Image);

         when 2 =>
            Log.Info ("Part 1: " & AOC2023_02.Sum_IDs_Of_Possible_Games'Image);

         -- TODO More days to come...

         when others =>
            Log.Error ("No solution yet for puzzles of day " & Number'Image);
      end case;
   end Solve_Day;

begin
   Util.Log.Loggers.Initialize ("log4j.properties");

   -- Solve_Day (1);
   Solve_Day (2);

   -- TODO More days to come...

end AOC2023;
