with AOC2024_02_Input; use AOC2024_02_Input;

package body AOC2024_02 with SPARK_Mode => Off is

   All_Reports : constant Reports := Parse_Input_Data (Input_File_Name);

   function Is_Safe (Report : Arrays.Unbound_Array) return Boolean;
   -- Determines whether a report is safe or not.
   -- It counts as safe if all values are either decreasing or increasing,
   -- and if any two adjacent values differ by at least one and at most three.

   function Solution_Part1 return Natural is
      Number_Of_Safe_Reports : Natural := 0;
   begin
      for I in All_Reports'Range loop
         if Is_Safe (All_Reports (I)) then
            Number_Of_Safe_Reports := @ + 1;
         end if;

         pragma Loop_Invariant (Number_Of_Safe_Reports <= I);
      end loop;

      return Number_Of_Safe_Reports;
   end Solution_Part1;

   function Solution_Part2 return Natural is
      Number_Of_Safe_Reports : Natural := 0;
   begin
      --  for Report of All_Reports loop
      --     if Is_Safe (Report) then
      --        Number_Of_Safe_Reports := @ + 1;
      --     else
      --        -- If the report is not safe, try removing any of the levels
      --        -- and see whether the modified report is safe.
      --        Try_Without_I : for I in Arrays.First_Index (Report) .. Arrays.Last_Index (Report) loop
      --           declare
      --              Length : constant Natural := Arrays.Length (Report);
      --              Modified_Report : Arrays.Unbound_Array := Arrays.To_Unbound_Array (Length - 1);
      --              Success : Boolean;
      --           begin
      --              -- Build modified report by copying all elements except the one at I.
      --              for J in Arrays.First_Index (Report) .. Arrays.Last_Index (Report) loop
      --                 if J /= I then
      --                    Arrays.Append (Modified_Report, Arrays.Element (Report, J), Success);
      --                 end if;
      --              end loop;
      --              -- Check whether the modified report is safe.
      --              if Is_Safe (Modified_Report) then
      --                 Number_Of_Safe_Reports := @ + 1;
      --                 exit Try_Without_I;
      --              end if;
      --           end;
      --        end loop Try_Without_I;
      --     end if;
      --  end loop;

      --  return Number_Of_Safe_Reports;
      return 0;
   end Solution_Part2;

   function Is_Safe (Report : Arrays.Unbound_Array) return Boolean is
      type Direction is (Increasing, Decreasing, None);

      Previous_Value : Natural;
      Current_Value  : Natural;

      Direction_So_Far : Direction := None;
   begin
      Previous_Value := Arrays.Element (Report, 1);

      for I in 2 .. Arrays.Length (Report) loop
         pragma Assume (I <= 10);

         Current_Value := Arrays.Element (Report, Levels (I));

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
      end loop;

      -- If we haven't found a violation yet, then the report is safe.
      return True;
   end Is_Safe;

end AOC2024_02;