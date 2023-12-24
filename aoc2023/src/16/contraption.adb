with Ada.Text_IO;

package body Contraption is

   function Parse_Input (File_Name : in String) return Grid is
      Input  : Ada.Text_IO.File_Type;
      Parsed : Grid;
      Row    : Rows := 1;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => File_Name);

      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line   : constant String := Ada.Text_IO.Get_Line (Input);
            Column : Columns := 1;
         begin
            for Char of Line loop
               declare
                  Tile : Tile_Type;
                  Cell : Grid_Cell;
               begin
                  Tile := (case Char is
                     when '.' => Empty_Space,
                     when '-' => Split_Horizontal,
                     when '|' => Split_Vertical,
                     when '/' => Mirror_Tilted_Right,
                     when '\' => Mirror_Tilted_Left,
                     when others => raise Unexpected_Input with "Unknown tile: " & Char
                  );
                  Cell := (Tile, Energized => False, Visited_From => [others => False]);
                  Parsed (Row, Column) := Cell;
               end;

               exit when Column = Columns'Last;
               Column := @ + 1;
            end loop;

            exit when Row = Rows'Last;
            Row := @ + 1;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Parsed;
   end Parse_Input;

end Contraption;