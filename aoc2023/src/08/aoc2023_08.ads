--------------------------------------
-- https://adventofcode.com/2023/day/8
--------------------------------------

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

package AOC2023_08 is

   Input_File_Name : constant String := "src/08/input_08.txt";

   -- Part 1
   function Steps_From_AAA_To_ZZZ return Natural;

private

   Unexpected_Input_Exception : exception;

   -- Instructions

   type Direction is (Left, Right);

   package Direction_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Direction
   );
   use Direction_Vectors;

   subtype Instructions is Vector;

   -- Network

   subtype Node is String (1 .. 3);

   type Branch is record
      Left  : Node;
      Right : Node;
   end record;

   package Networks is new Ada.Containers.Hashed_Maps (
      Key_Type        => Node,
      Element_Type    => Branch,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "="
   );

   subtype Network is Networks.Map;

end AOC2023_08;