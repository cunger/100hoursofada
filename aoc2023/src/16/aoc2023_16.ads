package AOC2023_16 is

   -- Part 1
   function Number_Of_Energized_Tiles return Natural;

private

   Input_File_Name  : constant String := "src/16/input_16.txt";

   Unexpected_Input : exception;

   type Tile is (
      Empty_Space,
      Split_Horizontal,
      Split_Vertical,
      Mirror_Tilted_Right,
      Mirror_Tilted_Left
   );

   type Rows    is range 1 .. 110;
   type Columns is range 1 .. 110;

   type Grid is array (Rows'Range, Columns'Range) of Tile;

   function Parse_Tile (Char : in Character) return Tile;
   function Parse_Input return Grid;

end AOC2023_16;