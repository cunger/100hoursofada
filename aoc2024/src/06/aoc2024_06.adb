with AOC2024_06_Input; use AOC2024_06_Input;
with Ada.Text_IO;

package body AOC2024_06 with SPARK_Mode => On is

   Initial_Map : Map := Parse_Input_Data (Input_File_Name);

   procedure Let_Guard_Patrol;
   -- Move the guard according to the rules until she leaves the map,
   -- and update each visited cell.

   function Number_Of_Visited_Cells (M : Map) return Natural;
   -- Count the number of unique positions that the guard visited.

   function Solution_Part1 return Natural is
   begin
      Let_Guard_Patrol;
      return Number_Of_Visited_Cells (Initial_Map);
   end Solution_Part1;

   function Solution_Part2 return Natural is
   begin
      return 0;
   end Solution_Part2;

   procedure Let_Guard_Patrol is
      Current_Direction : Direction;
      Current_Position  : Coordinate;
      Next_Position     : Coordinate;
   begin
      -- Find current position and direction of guard.
      for X in Initial_Map'Range (1) loop
         for Y in Initial_Map'Range (2) loop
            case Initial_Map (X, Y) is
               when Guard_Facing_Left =>
                  Current_Position  := (X, Y);
                  Current_Direction := Left;
               when Guard_Facing_Right =>
                  Current_Position  := (X, Y);
                  Current_Direction := Right;
               when Guard_Facing_Up =>
                  Current_Position  := (X, Y);
                  Current_Direction := Up;
               when Guard_Facing_Down =>
                  Current_Position  := (X, Y);
                  Current_Direction := Down;
               when others =>
                  null;
            end case;
         end loop;
      end loop;

      -- Move the guard until she leaves the map:
      -- If the guard is facing an obstacle, turn right 90 degrees.
      -- Otherwise keep walking straight ahead.
      Move_Guard : loop
         Next_Position := Move (Current_Position, Current_Direction);

         if Is_Off_The_Map (Next_Position) then
            exit Move_Guard;
         end if;

         if Initial_Map (Next_Position.X, Next_Position.Y) = Obstacle then
            Current_Direction := Turn_Right (Current_Direction);
         else
            Current_Position := Next_Position;
            Initial_Map (Current_Position.X, Current_Position.Y) := Visited;
         end if;
      end loop Move_Guard;
   end Let_Guard_Patrol;

   function Number_Of_Visited_Cells (M : Map) return Natural is
      Count : Natural := 0;
   begin
      for Cell of M when Cell = Visited loop
         Count := @ + 1;
      end loop;

      return Count;
   end Number_Of_Visited_Cells;

   function Move (Position : Coordinate; Dir : Direction) return Coordinate is
   begin
      case Dir is
         when Left =>
            return (Position.X, Position.Y - 1);
         when Right =>
            return (Position.X, Position.Y + 1);
         when Up =>
            return (Position.X - 1, Position.Y);
         when Down =>
            return (Position.X + 1, Position.Y);
      end case;
   end Move;

   function Turn_Right (Dir : Direction) return Direction is
   begin
      case Dir is
         when Left =>
            return Up;
         when Right =>
            return Down;
         when Up =>
            return Right;
         when Down =>
            return Left;
      end case;
   end Turn_Right;

   function Is_Off_The_Map (Position : Coordinate) return Boolean is
   begin
      return Position.X = 0 or Position.Y = 0 or Position.X = 131 or Position.Y = 131;
   end Is_Off_The_Map;

end AOC2024_06;