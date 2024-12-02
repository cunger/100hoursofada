with Spark_Unbound.Arrays;

package AOC2024_02_Input with SPARK_Mode => Off is

   type Levels is range 1 .. 10;
   -- There are never more than 10 levels in the input.

   package Arrays is new Spark_Unbound.Arrays (
      Index_Type   => Levels,
      Element_Type => Natural
   );

   type Reports is array (1 .. 1000) of Arrays.Unbound_Array;
   -- The type of the data contained in the input file.
   -- The file has 1000 lines, so we can fix the array size.

   function Parse_Input_Data (File_Name : in String) return Reports;

end AOC2024_02_Input;