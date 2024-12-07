with Ada.Text_IO;

package body AOC2024_07_Input with SPARK_Mode => Off is
   use Natural_Vectors;

   function Parse_Input_Data (File_Name : in String) return Input_Data is
      Input : Ada.Text_IO.File_Type;
      Line_Number : Positive := 1;
      Parsed_Data : Input_Data;
   begin
      -- Read the input file.
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => File_Name);

      -- Walk over each line, reading the report contained in that line.
      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line : constant String := Ada.Text_IO.Get_Line (Input);
            Line_Data : Data;

            -- Read each line character by character.
            -- If we encounter a colon, space or the end of the line,
            -- read the value from the last space up to the current index.
            Index_Of_Last_Space : Natural := 0;
         begin
            for I in Line'Range loop
               -- Read result from start to first colon.
               if Line (I) = ':' then
                  Line_Data.Result := Long_Long_Integer'Value (Line (1 .. I - 1));
               end if;

               -- After that, read values.
               if Line (I) = ' ' or I = Line'Last then
                  -- Skip space after colon.
                  if Line (I - 1) = ':' then
                     Index_Of_Last_Space := I;
                  else
                     declare
                        Start_Index : constant Positive := Index_Of_Last_Space + 1;
                        End_Index   : constant Positive := (if I = Line'Last then I else I - 1);
                        Value       : constant String := Line (Start_Index .. End_Index);
                     begin
                        Append (Line_Data.Values, Long_Long_Integer'Value (Value));
                        Index_Of_Last_Space := I;
                     end;
                  end if;
               end if;
            end loop;

            Parsed_Data (Line_Number) := Line_Data;
         end Process_Line;

         Line_Number := @ + 1;
      end loop;

      Ada.Text_IO.Close (Input);

      return Parsed_Data;
   end Parse_Input_Data;

end AOC2024_07_Input;