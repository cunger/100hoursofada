with AOC2021_01_Input; use AOC2021_01_Input;

package body AOC2021_01 with SPARK_Mode => On is

   function Number_Of_Increases (Values : in Measurements) return Natural with
      Global  => null,
      Depends => (Number_Of_Increases'Result => Values);

   function Solution_Part1 return Natural is
      Values : constant Measurements := Parse_Input_Data (Input_File_Name);
   begin
      return Number_Of_Increases (Values);
   end Solution_Part1;

   function Number_Of_Increases (Values : in Measurements) return Natural is
      Count : Natural := 0;
      Previous_Value : Natural := 0;
   begin
      for I in Values'Range loop
         declare
            Value : constant Natural := Values (I);
         begin
            if I > 1 and Value > Previous_Value then
               Count := Count + 1;
            end if;
            Previous_Value := Value;
         end;
      end loop;

      return Count;
   end Number_Of_Increases;

end AOC2021_01;