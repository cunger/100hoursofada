with Ada.Containers.Generic_Constrained_Array_Sort;

package AOC2024_01_Input with
   SPARK_Mode => On
is

   Number_Of_Lines : constant Natural := 1000;
   -- The number of lines in the input file.

   type Line_Number is range 1 .. Number_Of_Lines;

   type Location_List  is array (Line_Number'Range) of Natural;
   type Location_Lists is
      record
         Left  : Location_List;
         Right : Location_List;
      end record;

   function Parse_Input_Data (File_Name : in String) return Location_Lists;

   procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort (
      Index_Type   => Line_Number,
      Element_Type => Natural,
      Array_Type   => Location_List
   );

end AOC2024_01_Input;