package AOC2023_15 is

   Input_File_Name : constant String := "src/15/input_15.txt";

   -- Part 1
   function Hash_Sum_Of_Initialization_Sequence return Natural;

private

   subtype Hash_Value is Natural range 0 .. 255;

   function Hash_256 (Str : String) return Hash_Value;

end AOC2023_15;