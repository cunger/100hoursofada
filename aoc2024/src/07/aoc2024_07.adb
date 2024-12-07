with AOC2024_07_Input; use AOC2024_07_Input;

package body AOC2024_07 with SPARK_Mode => Off is
   use Natural_Vectors;

   All_Data : constant Input_Data := Parse_Input_Data (Input_File_Name);

   function Is_Possible (Data_Point : Data) return Boolean;

   function Solution_Part1 return Long_Long_Integer is
      Sum : Long_Long_Integer := 0;
   begin
      for Data_Point of All_Data when Is_Possible (Data_Point) loop
         Sum := @ + Data_Point.Result;
      end loop;

      return Sum;
   end Solution_Part1;

   function Solution_Part2 return Natural is
   begin
      return 0;
   end Solution_Part2;

   function Is_Possible (Data_Point : Data) return Boolean is
      Target_Result : constant Long_Long_Integer := Data_Point.Result;

      C : Cursor := First (Data_Point.Values);

      Current_Value    : Long_Long_Integer;
      Previous_Results : Vector := Empty_Vector;
      Current_Results  : Vector := Empty_Vector;
   begin
      -- Initialize previous results with the first value.
      Append (Previous_Results, Element (C));
      -- And move to the next value.
      C := Next (C);

      while Has_Element (C) loop
         Current_Value := Element (C);

         -- Try both + and * as next operator.
         for Previous_Result of Previous_Results loop
            Append (Current_Results, Previous_Result + Current_Value);
            Append (Current_Results, Previous_Result * Current_Value);
         end loop;

         -- Move to next value.
         C := Next (C);

         Previous_Results := Current_Results;
         Current_Results  := Empty_Vector;
      end loop;

      -- In the end, then check whether the target result was reached.
      for Result of Previous_Results loop
         if Result = Target_Result then
            Return True;
         end if;
      end loop;

      Return False;
   end Is_Possible;

end AOC2024_07;