package AOC2021_01_Input with
   SPARK_Mode => On
is

   Number_Of_Lines : constant Natural := 2000;
   -- The number of lines in the input file and thus
   -- the number of provided depth measurement values.

   type Measurements is array (1 .. Number_Of_Lines) of Natural;

   function Parse_Input_Data (File_Name : in String) return Measurements;

end AOC2021_01_Input;