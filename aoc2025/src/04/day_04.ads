--------------------------------------
-- https://adventofcode.com/2025/day/4
--------------------------------------
package Day_04 is

   -- Calculate and print the solution.
   procedure Solve;

private

   type Grid is array (0 .. 140, 0 .. 141) of Natural;
   -- 1 for roll of paper, 0 for empty grid cell.
   -- With an extra empty row at the top and bottom, and
   -- an extra empty colum left and right, for padding.

   -- Part 1
   function Number_Of_Accessible_Forklifts return Natural;

end Day_04;