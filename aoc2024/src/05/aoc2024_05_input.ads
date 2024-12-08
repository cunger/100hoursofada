with Ada.Containers.Vectors;

package AOC2024_05_Input with SPARK_Mode => Off is

   package Natural_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Natural
   );

   -- Types for ordering rules (first part of the input).

   type Order_Rule is record
      First  : Natural;
      Second : Natural;
   end record;

   type Order_Rules is array (1 .. 1176) of Order_Rule;

   -- Type for print updates (second part of the input).

   type Print_Updates is array (1 .. 202) of Natural_Vectors.Vector;

   -- Type for input data.

   type Input_Data is record
      Rules   : Order_Rules;
      Updates : Print_Updates;
   end record;

   function Parse_Input_Data (File_Name : in String) return Input_Data;

end AOC2024_05_Input;