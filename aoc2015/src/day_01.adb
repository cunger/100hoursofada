with Ada.Text_IO;

procedure Day_01 is

   package IO renames Ada.Text_IO;

   Input : IO.File_Type;

   Solution_Part1 : Integer := 0;
   Solution_Part2 : Natural := 0;

begin
   -- Open the input file in read mode.
   IO.Open (File => Input, Mode => IO.In_File, Name => "input/input_01.txt");

   -- Walk through the file line by line.
   while not IO.End_Of_File (Input) loop
      declare
         Line : constant String := IO.Get_Line (Input);
      begin
         for I in Line'Range loop
            declare
               Char : constant Character := Line (I);
            begin
               -- Move up one floor for each '('.
               if Char = '(' then
                  Solution_Part1 := Solution_Part1 + 1;
               end if;
               -- Move down one floor for each ')'.
               if Char = ')' then
                  Solution_Part1 := Solution_Part1 - 1;
               end if;
               -- Remember the first index where Santa enters the basement.
               if Solution_Part2 = 0 and Solution_Part1 < 0 then
                  Solution_Part2 := I;
               end if;
            end;
         end loop;
      end;
   end loop;

   -- Finally, close the file again.
   IO.Close (Input);

   IO.Put_Line ("Day 1 Part 1:" & Natural'Image (Solution_Part1));
   IO.Put_Line ("Day 1 Part 2:" & Natural'Image (Solution_Part2));

end Day_01;