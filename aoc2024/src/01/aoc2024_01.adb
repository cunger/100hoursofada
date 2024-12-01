with AOC2024_01_Input; use AOC2024_01_Input;

package body AOC2024_01 with SPARK_Mode => On is

   Lists : constant Location_Lists := Parse_Input_Data (Input_File_Name);

   function Solution_Part1 return Natural is
      Left_List  : Location_List := Lists.Left;
      Right_List : Location_List := Lists.Right;
      Sum        : Natural := 0;
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
            pragma Assert (Difference  < 100_000);

            Sum := @ + Difference;

            pragma Loop_Invariant (Sum < Natural (Index) * 100_000);
         end;
      end loop;

      return Sum;
   end Solution_Part1;

   function Count (Value : Natural; List : Location_List) return Natural
      with Post => Count'Result <= List'Length;

   function Solution_Part2 return Long_Natural is
      Score : Long_Natural := 0;
   begin
      -- Walk over the left list and multiply each value by the number of times
      -- it occurs in the right list.
      for Index in Line_Number'Range loop
         declare
            Value_On_The_Left       : constant Natural := Lists.Left (Index);
            Occurences_On_The_Right : constant Natural := Count (Value_On_The_Left, Lists.Right);
         begin
            pragma Assume (Value_On_The_Left < 100_000); -- True about the specific input file.
            pragma Assert (Occurences_On_The_Right <= 1000);
            pragma Assert (Value_On_The_Left * Occurences_On_The_Right <= 100_000_000);

            Score := @ + Long_Natural (Value_On_The_Left * Occurences_On_The_Right);

            pragma Loop_Invariant (Score <= Long_Natural (Index) * 100_000_000);
         end;
      end loop;

      return Score;
   end Solution_Part2;

   function Count (Value : Natural; List : Location_List) return Natural is
      Number_Of_Occurences : Natural := 0;
   begin
      for Index in List'Range loop
         if List (Index) = Value
         then
            Number_Of_Occurences := @ + 1;
         end if;

         pragma Loop_Invariant (Number_Of_Occurences <= Natural (Index));
      end loop;

      return Number_Of_Occurences;
   end Count;

end AOC2024_01;