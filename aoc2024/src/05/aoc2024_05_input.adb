with Ada.Text_IO;
with Ada.Strings.Fixed;

package body AOC2024_05_Input with SPARK_Mode => Off is
   use Ada.Strings.Fixed;
   use Natural_Vectors;

   type Input_Part is (Part1, Part2);

   function Parse_Input_Data (File_Name : in String) return Input_Data is
      Input      : Ada.Text_IO.File_Type;
      Data       : Input_Data;
      Part       : Input_Part := Part1;
      List_Index : Natural    := 0;
   begin
      -- Read the input file.
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => File_Name);

      -- Walk over each line, reading the report contained in that line.
      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line : constant String := Ada.Text_IO.Get_Line (Input);
         begin
            List_Index := @ + 1;

            if Line = "" then
               Part := Part2;
               List_Index := 0;
            elsif Part = Part1 then
               -- Read ordering rules.
               declare
                  Index_Of_Separator : Natural;
                  First_Page_Number  : Natural;
                  Second_Page_Number : Natural;
               begin
                  Index_Of_Separator := Index (Source => Line, Pattern => "|", From => 1);
                  First_Page_Number  := Natural'Value (Line (1 .. Index_Of_Separator - 1));
                  Second_Page_Number := Natural'Value (Line (Index_Of_Separator + 1 .. Line'Last));
                  Data.Rules (List_Index) := (First_Page_Number, Second_Page_Number);
               end;
            else
               -- Read print updates.
               declare
                  Prev_Comma : Natural := 1;
                  Next_Comma : Natural := 1;
                  Update     : Vector;
               begin
                  for I in 1 .. Count (Source => Line, Pattern => ",") loop
                     if I = 1 then
                        Next_Comma := Index (Source => Line, Pattern => ",", From => 1);
                        Append (Update, Natural'Value (Line (1 .. Next_Comma - 1)));
                     else
                        Next_Comma := Index (Source => Line, Pattern => ",", From => Prev_Comma + 1);
                        Append (Update, Natural'Value (Line (Prev_Comma + 1 .. Next_Comma - 1)));
                     end if;
                     Prev_Comma := Next_Comma;

                  end loop;
                  Append (Update, Natural'Value (Line (Prev_Comma + 1 .. Line'Last)));

                  Data.Updates (List_Index) := Update;
               end;
            end if;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Data;
   end Parse_Input_Data;

end AOC2024_05_Input;