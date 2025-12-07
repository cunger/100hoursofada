with Ada.Text_IO;

package body Day_07 is

   package IO renames Ada.Text_IO;

   Tachyon_Manifold : Grid := [others => [others => Empty]];
   Start_Position   : Coordinate;
   Number_Of_Splits : Integer := 0;
   Number_Of_Worlds : Big_Integer := 0;

   procedure Read_Input;
   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/07/input_07.txt");

      for Row in Dimension'Range loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Col in Dimension'Range loop
               declare
                  Char : constant Character := Line (Integer (Col));
               begin
                  if Char = 'S' then
                     Start_Position := (Row, Col);
                  elsif Char = '^' then
                     Tachyon_Manifold (Row, Col) := Splitter;
                  end if;
               end;
            end loop;
         end;
      end loop;

      IO.Close (Input);
   end Read_Input;

   procedure Send_Tachyon;
   procedure Send_Tachyon is
   begin
      -- There is a tachyon beam or particle at the start position.
      Tachyon_Manifold (Start_Position.Row, Start_Position.Col) := Visited;
      -- Then walk over each row and determine where tachyons are.
      for Row in Start_Position.Row + 1 .. Dimension'Last loop
         for Column in Dimension'Range loop
            -- Cell is hit by a tachyon, if there is one in the cell above.
            if Tachyon_Manifold (Row - 1, Column) = Visited then
               if Tachyon_Manifold (Row, Column) = Empty then
                  Tachyon_Manifold (Row, Column) := Visited;
               elsif Tachyon_Manifold (Row, Column) = Splitter then
                  -- If the cell is a splitter, we count it as a split.
                  Number_Of_Splits := @ + 1;
                  -- We also mark the cells left and right of it as visited.
                  if Column > Dimension'First then
                     Tachyon_Manifold (Row, Column - 1) := Visited;
                  end if;
                  if Column < Dimension'Last then
                     Tachyon_Manifold (Row, Column + 1) := Visited;
                  end if;
               end if;
            end if;
         end loop;
      end loop;
   end Send_Tachyon;

   procedure Compute_Timelines is
      Tachyon_Worlds : Worlds_Grid := [others => [others => 0]];
   begin
      for Row in reverse Dimension'Range loop
         for Column in Dimension'Range loop
            if Tachyon_Manifold (Row, Column) = Empty then
               if Row = Dimension'Last then
                  Tachyon_Worlds (Row, Column) := 1;
               else
                  Tachyon_Worlds (Row, Column) := Tachyon_Worlds (Row + 1, Column);
               end if;
            elsif Tachyon_Manifold (Row, Column) = Splitter then
               if Row = Dimension'Last then
                  Tachyon_Worlds (Row, Column) := 2;
               else
                  if Column > Dimension'First then
                     Tachyon_Worlds (Row, Column) := @ + Tachyon_Worlds (Row + 1, Column - 1);
                  end if;
                  if Column < Dimension'Last then
                     Tachyon_Worlds (Row, Column) := @ + Tachyon_Worlds (Row + 1, Column + 1);
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      Number_Of_Worlds := Tachyon_Worlds (Start_Position.Row, Start_Position.Col);
   end Compute_Timelines;

   procedure Solve is
   begin
      Read_Input;
      Compute_Timelines;
      Send_Tachyon;
      IO.Put_Line ("Part 1:" & Number_Of_Splits'Image);
      IO.Put_Line ("Part 2:" & To_String (Number_Of_Worlds));
   end Solve;

end Day_07;