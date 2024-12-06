package AOC2024_06_Input with SPARK_Mode => On is

   type Map_Cell is (
      Empty,
      Obstacle,
      Visited,
      Guard_Facing_Up,
      Guard_Facing_Down,
      Guard_Facing_Left,
      Guard_Facing_Right
   );

   type Map is array (1 .. 130, 1 .. 130) of Map_Cell;
   -- The type of the data contained in the input file.

   function Parse_Input_Data (File_Name : in String) return Map;
   -- Parse the initial map given in the input file.

end AOC2024_06_Input;