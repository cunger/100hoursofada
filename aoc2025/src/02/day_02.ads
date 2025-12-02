--------------------------------------
-- https://adventofcode.com/2025/day/2
--------------------------------------
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

package Day_02 is

   -- Calculate and print the solution.
   procedure Solve;

private

   -- Part 1 and 2
   function Sum_Invalid_Ids_1 return Big_Integer;
   function Sum_Invalid_Ids_2 return Big_Integer;

end Day_02;