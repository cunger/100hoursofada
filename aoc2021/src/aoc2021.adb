with Util.Log.Loggers;

with AOC2021_01;

procedure AOC2021 is

   type Day is new Positive range 1 .. 25;

   procedure Solve_Day (Number : Day);
   procedure Solve_Day (Number : Day) is
      Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("aoc2021");
   begin
      Log.Info ("Solving day" & Number'Image);

      case Number is
         when 1 =>
            Log.Info ("Part 1:" & AOC2021_01.Solution'Image);
            Log.Info ("Part 2: TODO");

         when others =>
            Log.Error ("No solution yet for puzzles of day" & Number'Image);
      end case;
   end Solve_Day;

begin
   Util.Log.Loggers.Initialize ("log4j.properties");

   Solve_Day (1);

end AOC2021;
