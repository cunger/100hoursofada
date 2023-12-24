package Contraption is

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

   function Parse_Input (File_Name : in String) return Grid;

   Unexpected_Input : exception;

end Contraption;