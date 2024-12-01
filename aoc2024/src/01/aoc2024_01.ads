--------------------------------------
-- https://adventofcode.com/2024/day/1
--------------------------------------

package AOC2024_01 with SPARK_Mode => On is

   subtype Long_Natural is Long_Integer range 0 .. Long_Integer'Last;

   function Solution_Part1 return Natural;
   function Solution_Part2 return Long_Natural;

private

   Input_File_Name : constant String := "src/01/input_01.txt";

end AOC2024_01;