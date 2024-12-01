with Ada.Text_IO;

package body AOC2024_01_Input with
   SPARK_Mode => Off
is

   function Index_Of (Char : Character; Input : String) return Integer;

   function Parse_Input_Data (File_Name : in String) return Location_Lists is
      Input : Ada.Text_IO.File_Type;
      Lists : Location_Lists;
      Index : Line_Number;
   begin
      -- Initiate the location lists and set the index to the start value.
      Lists := (Left => [others => 0], Right => [others => 0]);
      Index := 1;

      -- Read the input file.
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => File_Name);

      -- Walk over each line, reading left and right value and filling the location lists
      -- in the order in which they are given.
      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line        : constant String   := Ada.Text_IO.Get_Line (Input);
            Split_Index : constant Positive := Index_Of (' ', Line);
            Left_ID     : constant String   := Line (Line'First .. Split_Index - 1);
            Right_ID    : constant String   := Line (Split_Index + 3 .. Line'Last);
         begin
            Lists.Left (Index)  := Natural'Value (Left_ID);
            Lists.Right (Index) := Natural'Value (Right_ID);

            if Index < Line_Number'Last
            then
               Index := @ + 1;
            end if;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Lists;
   end Parse_Input_Data;

   function Index_Of (Char : Character; Input : String) return Integer is
   begin
      for Index in Input'Range loop
         if Input (Index) = Char
         then
            return Index;
         end if;
      end loop;

      return -1;
   end Index_Of;

end AOC2024_01_Input;