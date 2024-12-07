with Ada.Containers.Vectors;

package AOC2024_07_Input with SPARK_Mode => Off is

   package Natural_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Long_Long_Integer
   );

   type Data is record
      Result : Long_Long_Integer;
      Values : Natural_Vectors.Vector;
   end record;

   type Input_Data is array (1 .. 850) of Data;
   -- The type of the data contained in the input file.
   -- The file has 850 lines, so we can fix the array size.

   function Parse_Input_Data (File_Name : in String) return Input_Data;

end AOC2024_07_Input;