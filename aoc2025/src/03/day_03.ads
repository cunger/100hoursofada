--------------------------------------
-- https://adventofcode.com/2025/day/3
--------------------------------------
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

package Day_03 is

   -- Calculate and print the solution.
   procedure Solve;

private

   type Digit is new Natural range 0 .. 9;

   type Battery is record
      Index : Positive range 1 .. 100;
      Value : Digit;
   end record;

   type Battery_Bank is array (Positive range 1 .. 100) of Battery;

   -- Find the battery with the largest value in the range from start to end index. 
   function Find_Largest_Battery (Bank : Battery_Bank; Start_Index : Natural; End_Index : Natural) return Battery;

   -- Find the maximum joltage for each line and sum them up.
   function Sum_Joltages (Number_Of_Batteries : Natural) return Big_Integer;

end Day_03;