with Ada.Text_IO;

package body AOC2024_04_Input with SPARK_Mode => Off is

   function Parse_Input_Data (File_Name : in String) return Word_Search is
      File        : Ada.Text_IO.File_Type;
      Input       : Word_Search;
      Line_Number : Positive := 1;
   begin
      -- Read the input file.
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);

      -- Walk over each line, filling the rows in the word search matrix
      -- with the characters in that line.
      while not Ada.Text_IO.End_Of_File (File) loop
         Process_Line : declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            for Column in Line'First .. Line'Last loop
               Input (Line_Number, Column) := Line (Column);
            end loop;

            Line_Number := @ + 1;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (File);

      return Input;
   end Parse_Input_Data;

end AOC2024_04_Input;