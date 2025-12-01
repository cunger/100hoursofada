with Ada.Text_IO;

package body Day_01 is

   package IO renames Ada.Text_IO;

   procedure Process_Rotations is
      Input : IO.File_Type;
      Current_Position : Natural := 50;
   begin
      -- Open the input file in read mode.
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/01/input_01.txt");

      -- Walk through the file line by line.
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
            Direction : constant Character := Line (1);
            Steps : constant Natural := Natural'Value (Line (2 .. Line'Last));

            Raw : Integer;
            New_Passes : Natural;
            Prev_Position : Natural;
         begin
            Prev_Position := Current_Position;

            -- raw addition or substraction
            if Direction = 'L' then
               Raw := Current_Position - Steps;
            elsif Direction = 'R' then
               Raw := Current_Position + Steps;
            end if;

            -- determine new position within the 0 .. 99 range
            Current_Position := Raw mod 100;
            -- count if dial stops at zero
            if Current_Position = 0 then
               Stops_At_Zero := Stops_At_Zero + 1;
            end if;

            -- determine the number of times zero was passed
            New_Passes := abs (Raw / 100);
            -- there are two edge cases when New_Passes is too low
            if Raw = 0 then
               New_Passes := New_Passes + 1;
            end if;
            if Raw < 0 and Prev_Position > 0 then
               New_Passes := New_Passes + 1;
            end if;

            Passes_Over_Zero := Passes_Over_Zero + New_Passes;
         end;
      end loop;

      -- Finally, close the file again.
      IO.Close (Input);
   end Process_Rotations;

   procedure Solve is
   begin
      Process_Rotations;
      IO.Put_Line ("Part 1:" & Stops_At_Zero'Image);
      IO.Put_Line ("Part 2:" & Passes_Over_Zero'Image);
   end Solve;

end Day_01;