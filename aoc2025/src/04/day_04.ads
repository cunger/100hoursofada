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

   function Number_Of_Accessible_Rolls (G : in out Grid; Remove : Boolean) return Natural;
   function Number_Of_Recursively_Accessible_Rolls (G : in out Grid) return Natural;

end Day_04;