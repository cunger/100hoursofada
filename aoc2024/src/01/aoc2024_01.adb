with AOC2024_01_Input; use AOC2024_01_Input;

package body AOC2024_01 with SPARK_Mode => On is

   Lists : constant Location_Lists := Parse_Input_Data (Input_File_Name);

   function Solution_Part1 return Long_Natural is
      Left_List  : Location_List := Lists.Left;
      Right_List : Location_List := Lists.Right;
      Sum        : Long_Natural  := 0;
   begin
      -- Sort the lists.
      Sort (Left_List);
      Sort (Right_List);

      -- Walk over the lists, noting the difference between each two values.
      for Index in Line_Number'Range loop
         declare
            Left_Value  : constant Natural := Left_List (Index);
            Right_Value : constant Natural := Right_List (Index);
            Difference  : constant Natural := abs (Left_Value - Right_Value);
         begin
            pragma Assume (Left_Value  < 100_000); -- True about the specific input file.
            pragma Assume (Right_Value < 100_000); -- True about the specific input file.
            pragma Assert (Difference < 100_000);
            -- pragma Loop_Invariant (Sum <= Long_Natural (Index * 100_000));

            Sum := @ + Long_Natural (Difference);
         end;
      end loop;

      return Sum;
   end Solution_Part1;

   function Count (Value : Natural; List : Location_List) return Natural
   with Post => Count'Result <= Location_List'Length;

   function Solution_Part2 return Long_Natural is
      Score : Long_Natural := 0;
   begin
      -- Walk over the left list and multiply each value by the number of times
      -- it occurs in the right list.
      for Index in Line_Number'Range loop
         declare
            Value : constant Natural := Lists.Left (Index);
         begin
            pragma Assume (Value < 100_000); -- True about the specific input file.

            Score := @ + Long_Natural (Value * Count (Value, Lists.Right));
         end;
      end loop;

      return Score;
   end Solution_Part2;

   function Count (Value : Natural; List : Location_List) return Natural is
      Number_Of_Occurences : Natural := 0;
   begin
      for Index in List'Range loop
         pragma Loop_Invariant (Number_Of_Occurences <= Natural (Index));

         if List (Index) = Value
         then
            Number_Of_Occurences := @ + 1;
         end if;
      end loop;

      return Number_Of_Occurences;
   end Count;

end AOC2024_01;