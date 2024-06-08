with Ada.Text_IO;

package body AOC2021_01_Input is

   function Parse_Input_Data (File_Name : in String) return Depth_Measurements is
      Input  : Ada.Text_IO.File_Type;
      Index  : Natural := 1;
      Values : Depth_Measurements := [others => 0];
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => File_Name);

      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line  : constant String := Ada.Text_IO.Get_Line (Input);
            Value : Natural;
         begin
            Value := Integer'Value (Line);
            Values (Index) := Value;
            Index := @ + 1;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Values;
   end Parse_Input_Data;

end AOC2021_01_Input;