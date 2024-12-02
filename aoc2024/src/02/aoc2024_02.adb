with AOC2024_02_Input; use AOC2024_02_Input;
with Natural_Vectors;  use Natural_Vectors;

package body AOC2024_02 with SPARK_Mode => Off is

   All_Reports : constant Reports := Parse_Input_Data (Input_File_Name);

   function Is_Safe (Values : Report) return Boolean;
   -- Determines whether a report is safe or not.
   -- It counts as safe if all values are either decresing or increasing,
   -- and if any two adjacent values differ by at least one and at most three.

   function Solution_Part1 return Natural is
      Number_Of_Safe_Reports : Natural := 0;
   begin
      for R of All_Reports when Is_Safe (R) loop
         Number_Of_Safe_Reports := @ + 1;
      end loop;

      return Number_Of_Safe_Reports;
   end Solution_Part1;

   function Solution_Part2 return Natural is
      Number_Of_Safe_Reports : Natural := 0;
   begin
      for R of All_Reports loop
         if Is_Safe (R) then
            Number_Of_Safe_Reports := @ + 1;
         else
            -- If the report is not safe, try removing any of the levels
            -- and see whether the modified report is safe.
            Try_Without_I : for I in R.First_Index .. R.Last_Index loop
               declare
                  Modified_R : Report := R;
               begin
                  Modified_R.Delete (Index => I);
                  if Is_Safe (Modified_R) then
                     Number_Of_Safe_Reports := @ + 1;
                     exit Try_Without_I;
                  end if;
               end;
            end loop Try_Without_I;
         end if;
      end loop;

      return Number_Of_Safe_Reports;
   end Solution_Part2;

   function Is_Safe (Values : Report) return Boolean is
      type Direction is (Increasing, Decreasing, None);

      C : Cursor := First (Values);
      Previous_Value : Natural  := Element (C);
      Current_Value  : Natural;

      Direction_So_Far : Direction := None;
   begin
      C := Next (C);

      while Has_Element (C) loop
         Current_Value := Element (C);

         -- Any two adjacent levels need to differ by at least one and at most three.
         declare
            Difference : constant Natural := abs (Previous_Value - Current_Value);
         begin
            if Difference = 0 or Difference > 3 then
               return False;
            end if;
         end;

         -- If we don't know the direction yet, set it.
         if Direction_So_Far = None then
            Direction_So_Far := (if Previous_Value < Current_Value then Increasing else Decreasing);
         else
         -- Check that the current direction is according to the set one.
            declare
               Current_Direction : Direction;
            begin
               Current_Direction := (if Previous_Value < Current_Value then Increasing else Decreasing);
               if Current_Direction /= Direction_So_Far then
                  return False;
               end if;
            end;
         end if;

         -- If all is good, continue with the next value.
         Previous_Value := Current_Value;
         C := Next (C);
      end loop;

      -- If we haven't found a violation yet, then the report is safe.
      return True;
   end Is_Safe;

end AOC2024_02;