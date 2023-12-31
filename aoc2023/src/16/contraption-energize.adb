package body Contraption.Energize is

   function Send_Beam_Through (G : Grid; Row : Rows; Col : Columns; Dir : Direction)
      return Natural is

      -- Protected object holding the grid.
      protected Grid_State is
         procedure Init_From (Unenergized_Grid : Grid);
         procedure Energize (Row : Rows; Column : Columns; Dir : Direction);
         function  Get_Tile (Row : Rows; Column : Columns) return Tile_Type;
         function  Already_Visited (Row : Rows; Column : Columns; Dir : Direction) return Boolean;
         function  Number_Of_Energized_Cells return Natural;
      private
         G : Grid;
      end Grid_State;

      protected body Grid_State is
         procedure Init_From (Unenergized_Grid : Grid) is
         begin
            G := Unenergized_Grid;
         end Init_From;

         procedure Energize (Row : Rows; Column : Columns; Dir : Direction) is
         begin
            G (Row, Column).Energized := True;
            G (Row, Column).Visited_From (Dir) := True;
         end Energize;

         function Get_Tile (Row : Rows; Column : Columns) return Tile_Type is
         begin
            return G (Row, Column).Tile;
         end Get_Tile;

         function Already_Visited (Row : Rows; Column : Columns; Dir : Direction) return Boolean is
         begin
            return G (Row, Column).Visited_From (Dir);
         end Already_Visited;

         function Number_Of_Energized_Cells return Natural is
            Count : Natural := 0;
         begin
            for I in Rows'Range loop
               for J in Columns'Range loop
                  if G (I, J).Energized then
                     Count := @ + 1;
                  end if;
               end loop;
            end loop;

            return Count;
         end Number_Of_Energized_Cells;
      end Grid_State;

      procedure Spawn_Beam (Row : Rows; Col : Columns; Dir : Direction);

      task type Beam (Start_Row : Rows; Start_Col : Columns; Start_Dir : Direction);
      task body Beam is
         Row : Rows      := Start_Row;
         Col : Columns   := Start_Col;
         Dir : Direction := Start_Dir;
      begin
         Step : loop
            exit Step when Grid_State.Already_Visited (Row, Col, Dir);

            Grid_State.Energize (Row, Col, Dir);

            -- Based on the tile, decide where the beam is moving.
            case Grid_State.Get_Tile (Row, Col) is
               when Empty_Space => null;
               -- Just passing through, not changing direction.

               when Mirror_Tilted_Right =>
               -- Reflect based on the direction the beam is coming from.
               case Dir is
                  when Right => Dir := Up;
                  when Left  => Dir := Down;
                  when Down  => Dir := Left;
                  when Up    => Dir := Right;
               end case;

               when Mirror_Tilted_Left =>
               -- Reflect based on the direction the beam is coming from.
               case Dir is
                  when Right => Dir := Down;
                  when Left  => Dir := Up;
                  when Down  => Dir := Right;
                  when Up    => Dir := Left;
               end case;

               when Split_Horizontal =>
               -- If coming from left or right, just pass through.
               -- Otherwise split beam: spawn two new beams (one
               -- to the left and on to the right) and stop this one.
               if Dir = Up or Dir = Down then
                  Spawn_Beam (Row, Col, Left);
                  Spawn_Beam (Row, Col, Right);
                  exit Step;
               end if;

               when Split_Vertical =>
               -- If coming from top or bottom, just pass through.
               -- Otherwise split beam: spawn two new beams (one
               -- going up and on going down) and stop this one.
               if Dir = Left or Dir = Right then
                  Spawn_Beam (Row, Col, Up);
                  Spawn_Beam (Row, Col, Down);
                  exit Step;
               end if;
            end case;

            exit Step when Dir = Right and Col = Columns'Last;
            exit Step when Dir = Left  and Col = Columns'First;
            exit Step when Dir = Down  and Row = Rows'Last;
            exit Step when Dir = Up    and Row = Rows'First;

            case Dir is
               when Right => Col := @ + 1;
               when Left  => Col := @ - 1;
               when Down  => Row := @ + 1;
               when Up    => Row := @ - 1;
            end case;
         end loop Step;
      end Beam;

      procedure Spawn_Beam (Row : Rows; Col : Columns; Dir : Direction) is
         New_Beam : Beam (Row, Col, Dir);
      begin
         null;
      end Spawn_Beam;

   begin

      Grid_State.Init_From (G);

      Spawn_Beam (Row, Col, Dir);

      return Grid_State.Number_Of_Energized_Cells;

   end Send_Beam_Through;

end Contraption.Energize;