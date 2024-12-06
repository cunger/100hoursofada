with Ada.Text_IO;

package body AOC2024_06_Input with SPARK_Mode => Off is

   function Parse_Input_Data (File_Name : in String) return Map is
      Input       : Ada.Text_IO.File_Type;
      Line_Number : Positive := 1;
      Initial_Map : Map;
   begin
      -- Read the input file.
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => File_Name);

      -- Walk over each line, reading the report contained in that line.
      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line : constant String := Ada.Text_IO.Get_Line (Input);
         begin
            for I in Line'Range loop
               declare
                  Char : constant Character := Line (I);
                  Cell : Map_Cell;
               begin
                  case Char is
                     when '.' =>
                        Cell := Empty;
                     when '#' =>
                        Cell := Obstacle;
                     when '^' =>
                        Cell := Guard_Facing_Up;
                     when '>' =>
                        Cell := Guard_Facing_Right;
                     when 'v' =>
                        Cell := Guard_Facing_Down;
                     when '<' =>
                        Cell := Guard_Facing_Left;
                     when others =>
                        Cell := Empty;
                  end case;
                  Initial_Map (Line_Number, I) := Cell;
               end;
            end loop;

            Line_Number := @ + 1;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Initial_Map;
   end Parse_Input_Data;

end AOC2024_06_Input;