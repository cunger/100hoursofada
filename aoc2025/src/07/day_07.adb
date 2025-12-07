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
      Tachyon_Worlds : Worlds_Grid := [others => [others => 0]];
      -- We count the number of worlds in a kind of Pascal triangle.
   begin
      -- There is a tachyon beam or particle at the start position.
      Tachyon_Manifold (Start_Position.Row, Start_Position.Col) := Visited;
      Tachyon_Worlds   (Start_Position.Row, Start_Position.Col) := 1;
      -- Then walk over each row and determine where tachyons are.
      for Row in Start_Position.Row + 1 .. Dimension'Last loop
         for Column in Dimension'Range loop
            -- Cell is hit by a tachyon, if there is one in the cell above.
            if Tachyon_Manifold (Row - 1, Column) = Visited then
               if Tachyon_Manifold (Row, Column) = Empty then
                  Tachyon_Manifold (Row, Column) := Visited;
                  Tachyon_Worlds   (Row, Column) := Tachyon_Worlds (Row - 1, Column);
                  -- No splitting, no new worlds are created.
               elsif Tachyon_Manifold (Row, Column) = Splitter then
                  -- If the cell is a splitter, we count it as a split.
                  Number_Of_Splits := @ + 1;
                  -- We also mark the cells left and right of it as visited,
                  -- and create a new world in each of them.
                  if Column > Dimension'First then
                     Tachyon_Manifold (Row, Column - 1) := Visited;
                     Tachyon_Worlds   (Row, Column - 1) := @ + Tachyon_Worlds (Row - 1, Column);
                  end if;
                  if Column < Dimension'Last then
                     Tachyon_Manifold (Row, Column + 1) := Visited;
                     Tachyon_Worlds   (Row, Column + 1) := @ + Tachyon_Worlds (Row - 1, Column);
                  end if;
               end if;
            end if;
         end loop;

         -- Print row for debugging
         --  for Col in Dimension'Range loop
         --     IO.Put (Tachyon_Worlds (Row, Col)'Image);
         --  end loop;
         --  IO.New_Line;
      end loop;
   
      -- Sum worlds in the last row.
      for Column in Dimension'Range loop
         Number_Of_Worlds := @ + Tachyon_Worlds (Dimension'Last, Column);
      end loop;
   end Send_Tachyon;

   procedure Solve is
   begin
      Read_Input;
      Send_Tachyon;
      IO.Put_Line ("Part 1:" & Number_Of_Splits'Image);
      IO.Put_Line ("Part 2:" & To_String (Number_Of_Worlds));
   end Solve;

end Day_07;