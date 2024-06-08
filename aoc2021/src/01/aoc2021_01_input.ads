package AOC2021_01_Input is

   Number_Of_Lines : constant Natural := 2000;
   -- The number of lines in the input file and thus
   -- the number of provided depth measurement values.

   type Depth_Measurements is array (1 .. Number_Of_Lines) of Natural;

   function Parse_Input_Data (File_Name : in String) return Depth_Measurements with
      Post => (for all I in Parse_Input_Data'Result'Range => Parse_Input_Data'Result (I) > 0);

end AOC2021_01_Input;