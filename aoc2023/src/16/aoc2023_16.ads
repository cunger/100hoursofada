package AOC2023_16 is

   -- Part 1
   function Number_Of_Energized_Tiles return Natural;

   -- Part 2
   function Max_Number_Of_Energized_Tiles return Natural;

private

   Input_File_Name  : constant String := "src/16/input_16.txt";

   Unexpected_Input : exception;

   type Tile_Type is (
      Empty_Space,
      Split_Horizontal,
      Split_Vertical,
      Mirror_Tilted_Right,
      Mirror_Tilted_Left
   );

   type Direction is (Right, Left, Up, Down);

   type Cache is array (Direction) of Boolean;

   type Grid_Cell is record
      Tile         : Tile_Type;
      Energized    : Boolean;
      Visited_From : Cache;
   end record;

   type Rows    is range 1 .. 110;
   type Columns is range 1 .. 110;

   type Grid is array (Rows'Range, Columns'Range) of Grid_Cell;

   function Parse_Input return Grid;

end AOC2023_16;