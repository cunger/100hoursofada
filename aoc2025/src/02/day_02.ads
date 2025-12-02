--------------------------------------
-- https://adventofcode.com/2025/day/2
--------------------------------------
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

package Day_02 is

   -- Calculate and print the solution.
   procedure Solve;

private

   -- Compute solution for part 1.
   function Sum_Valid_Ids return Big_Integer;

end Day_02;