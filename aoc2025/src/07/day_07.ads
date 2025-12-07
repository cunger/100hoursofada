--------------------------------------
-- https://adventofcode.com/2025/day/7
--------------------------------------
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

package Day_07 is

   -- Calculate and print the solution.
   procedure Solve;

private

   type Dimension is range 1 .. 141;

   type Cell is (Empty, Splitter, Visited);
   type Grid is array (Dimension, Dimension) of Cell;
   type Worlds_Grid is array (Dimension, Dimension) of Big_Integer;

   type Coordinate is record
      Row : Dimension;
      Col : Dimension;
   end record;

end Day_07;