--------------------------------------
-- https://adventofcode.com/2024/day/6
--------------------------------------

package AOC2024_06 with SPARK_Mode => On is

   function Solution_Part1 return Natural;
   function Solution_Part2 return Natural;

private

   Input_File_Name : constant String := "src/06/input_06.txt";

   type Coordinate is record
      X, Y : Integer range 0 .. 131;
   end record;
   -- X specifies the row, Y the column.
   -- We allow the coordinate to leave the map by one position.

   type Direction is (Up, Down, Left, Right);

   function Move (Position : Coordinate; Dir : Direction) return Coordinate;
   -- Move the given coordinate into the given direction.

   function Turn_Right (Dir : Direction) return Direction;
   -- Turn right by 90 degrees.

   function Is_Off_The_Map (Position : Coordinate) return Boolean;

end AOC2024_06;