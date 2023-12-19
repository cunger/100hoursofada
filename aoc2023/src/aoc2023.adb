with Util.Log.Loggers;

with AOC2023_01;
with AOC2023_02;
with AOC2023_04;
with AOC2023_07;
with AOC2023_08;
with AOC2023_15;

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
            Log.Info ("Part 2: " & AOC2023_02.Sum_Powers_Of_Minimal_Sets'Image);

         when 4 =>
            Log.Info ("Part 1: " & AOC2023_04.Total_Number_Of_Points'Image);
            Log.Info ("Part 2: " & AOC2023_04.Total_Number_Of_Cards'Image);

         when 7 =>
            Log.Info ("Part 1: " & AOC2023_07.Total_Winnings (With_Joker => False)'Image);
            Log.Info ("Part 2: " & AOC2023_07.Total_Winnings (With_Joker => True)'Image);

         when 8 =>
            Log.Info ("Part 1: " & AOC2023_08.Steps_From_AAA_To_ZZZ'Image);

         when 15 =>
            Log.Info ("Part 1: " & AOC2023_15.Hash_Sum_Of_Initialization_Sequence'Image);
            Log.Info ("Part 2: " & AOC2023_15.Total_Focusing_Power'Image);

         -- TODO More days to come...

         when others =>
            Log.Error ("No solution yet for puzzles of day " & Number'Image);
      end case;
   end Solve_Day;

begin
   Util.Log.Loggers.Initialize ("log4j.properties");

   Solve_Day (1);
   Solve_Day (2);
   Solve_Day (4);
   Solve_Day (7);
   Solve_Day (8);
   Solve_Day (15);

   -- TODO More days to come...

end AOC2023;
