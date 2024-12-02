with Natural_Vectors;

package AOC2024_02_Input with
   SPARK_Mode => On
is

   subtype Report is Natural_Vectors.Vector;

   type Reports is array (1 .. 1000) of Report;
   -- The type of the data contained in the input file.
   -- The file has 1000 lines, so we can fix the array size.

   function Parse_Input_Data (File_Name : in String) return Reports;

end AOC2024_02_Input;