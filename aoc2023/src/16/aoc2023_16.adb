with Ada.Text_IO;

package body AOC2023_16 is

   -- Part 1
   function Number_Of_Energized_Tiles return Natural is
      Contraption : Grid;
   begin
      Contraption := Parse_Input;

      return 0;
   end Number_Of_Energized_Tiles;

   function Parse_Tile (Char : in Character) return Tile is
   begin
      return (case Char is
         when '.' => Empty_Space,
         when '-' => Split_Horizontal,
         when '|' => Split_Vertical,
         when '/' => Mirror_Tilted_Right,
         when '\' => Mirror_Tilted_Left,
         when others => raise Unexpected_Input with "Unknown tile: " & Char);
   end Parse_Tile;

   function Parse_Input return Grid is
      Input  : Ada.Text_IO.File_Type;
      Parsed : Grid := [others => [others => Empty_Space]];
      Row    : Rows := 1;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line   : String  := Ada.Text_IO.Get_Line (Input);
            Column : Columns := 1;
         begin
            for Char of Line loop
               Parsed (Row, Column) := Parse_Tile (Char);
               
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

end AOC2023_16;