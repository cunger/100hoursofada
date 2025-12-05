--------------------------------------
-- https://adventofcode.com/2025/day/5
--------------------------------------
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

package Day_05 is

   -- Calculate and print the solution.
   procedure Solve;

private

   type Ingredient_Range is record
      Lower_Id : Big_Integer;
      Upper_Id : Big_Integer;
   end record;

   type Ingredient_Ranges is array (Positive range 1 .. 182) of Ingredient_Range;

   type Ingredient_Ids is array (Positive range 1 .. 1000) of Big_Integer;

end Day_05;