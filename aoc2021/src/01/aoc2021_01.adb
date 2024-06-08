with AOC2021_01_Input; use AOC2021_01_Input;

package body AOC2021_01 is

   function Number_Of_Depth_Increases (Values : in Depth_Measurements) return Natural;

   function Solution return Natural is
      Values : constant Depth_Measurements := Parse_Input_Data (Input_File_Name);
   begin
      return Number_Of_Depth_Increases (Values);
   end Solution;

   function Number_Of_Depth_Increases (Values : in Depth_Measurements) return Natural is
      Count : Natural := 0;
      Previous_Value : Natural := 0;
   begin
      for I in Values'First .. Values'Last loop
         declare
            Value : constant Natural := Values (I);
         begin
            if I > 1 and Value > Previous_Value
            then
               Count := @ + 1;
            end if;
            Previous_Value := Value;
         end;
      end loop;

      return Count;
   end Number_Of_Depth_Increases;

end AOC2021_01;