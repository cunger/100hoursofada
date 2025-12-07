--------------------------------------
-- https://adventofcode.com/2025/day/6
--------------------------------------
package Day_06 is

   -- Calculate and print the solution.
   procedure Solve;

private

   type Operation is (Sum, Multiplication, None);

   type Homework_Task is record
      N1 : Integer;
      N2 : Integer;
      N3 : Integer;
      N4 : Integer;
      Op : Operation;
   end record;

   type Homework_Tasks is array (Positive range 1 .. 1000) of Homework_Task;

end Day_06;