package AOC2024_04_Input with SPARK_Mode => Off is

   type Dimension is range 1 .. 140;
   -- The dimension of the data contained in the input file.

   type Word_Search is array (Dimension'Range, Dimension'Range) of Character;
   -- The characters in the input file as a two-dimensional array.

   function Parse_Input_Data (File_Name : in String) return Word_Search;

end AOC2024_04_Input;