with Ada.Text_IO;

package body AOC2024_02_Input with SPARK_Mode => Off is

   function Parse_Input_Data (File_Name : in String) return Reports is
      Input       : Ada.Text_IO.File_Type;
      Line_Number : Positive := 1;
      All_Reports : Reports;
   begin
      -- Read the input file.
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => File_Name);

      -- Walk over each line, reading the report contained in that line.
      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line : constant String := Ada.Text_IO.Get_Line (Input);
            Current_Report : Arrays.Unbound_Array := Arrays.To_Unbound_Array (2);

            -- Read each line character by character.
            -- If we encounter a space or the end of the line,
            -- read the value from the last space up to the current index.
            Index_Of_Last_Space : Natural := 0;
         begin
            for I in Line'Range loop
               if Line (I) = ' ' or I = Line'Last then
                  declare
                     Start_Index : constant Positive := Index_Of_Last_Space + 1;
                     End_Index   : constant Positive := (if I = Line'Last then I else I - 1);
                     Value       : constant String := Line (Start_Index .. End_Index);
                     Success     : Boolean;
                  begin
                     Arrays.Append (Current_Report, Natural'Value (Value), Success);
                     pragma Assume (Success);

                     Index_Of_Last_Space := I;
                  end;
               end if;
            end loop;

            All_Reports (Line_Number) := Current_Report;
         end Process_Line;

         Line_Number := @ + 1;
      end loop;

      Ada.Text_IO.Close (Input);

      return All_Reports;
   end Parse_Input_Data;

end AOC2024_02_Input;