--------------------------------------
-- https://adventofcode.com/2025/day/1
--------------------------------------
package Day_01 is

   -- Calculate and print the solution.
   procedure Solve;

private

   -- Solution of part 1 is the number of times that rotation stops at 0.
   Stops_At_Zero : Natural := 0;

   -- Solution of part 2 is the number of times that the dial points to 0.
   Passes_Over_Zero : Natural := 0;

   -- Procedure that reads the input and executes the defined rotations.
   -- Sets Stops_At_Zero and Passes_Over_Zero.
   procedure Process_Rotations;

end Day_01;