--------------------------------------
-- https://adventofcode.com/2023/day/2
--------------------------------------

package AOC2023_02 is

   Input_File_Name : constant String := "src/02/input_02.txt";

   function Sum_IDs_Of_Possible_Games return Natural;

private

   Max_Red   : constant Natural := 12;
   Max_Blue  : constant Natural := 14;
   Max_Green : constant Natural := 13;

   type Draw is record
      Red   : Natural;
      Blue  : Natural;
      Green : Natural;
   end record;

   Unexpected_Input_Format : exception;

end AOC2023_02;