package AOC2024_04_Input with SPARK_Mode => Off is

   type Word_Search is array (1 .. 140, 1 .. 140) of Character;
   -- The type of the data contained in the input file.

   function Parse_Input_Data (File_Name : in String) return Word_Search;

end AOC2024_04_Input;