with AOC2021_01_Input; use AOC2021_01_Input;

package body AOC2021_01 with SPARK_Mode => On is

   function Number_Of_Depth_Increases (Values : in Depth_Measurements) return Natural with
      Global  => null,
      Depends => (Number_Of_Depth_Increases'Result => Values),
      Post    => Number_Of_Depth_Increases'Result < Values'Size;

   function Solution_Part1 return Natural is
      Values : constant Depth_Measurements := Parse_Input_Data (Input_File_Name);
   begin
      return Number_Of_Depth_Increases (Values);
   end Solution_Part1;

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
               Count := Count + 1;
            end if;
            Previous_Value := Value;
         end;

         pragma Loop_Invariant (Count >= 0 and Count < Values'Size);
      end loop;

      return Count;
   end Number_Of_Depth_Increases;

end AOC2021_01;