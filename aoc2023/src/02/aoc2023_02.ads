--------------------------------------
-- https://adventofcode.com/2023/day/2
--------------------------------------

package AOC2023_02 is

   -- Part 1
   function Sum_IDs_Of_Possible_Games return Natural;

   -- Part 2
   function Sum_Powers_Of_Minimal_Sets return Natural;

private

   Input_File_Name : constant String := "src/02/input_02.txt";

   Max_Red   : constant Natural := 12;
   Max_Blue  : constant Natural := 14;
   Max_Green : constant Natural := 13;

   type Draw is record
      Red   : Natural;
      Blue  : Natural;
      Green : Natural;
   end record;

end AOC2023_02;