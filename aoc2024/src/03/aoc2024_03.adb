with Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;

package body AOC2024_03 with SPARK_Mode => Off is

   function Solution_Part1 return Natural is
      Pattern : constant Pattern_Matcher := Compile ("mul\((\d+),(\d+)\)");
      Input   : Ada.Text_IO.File_Type;
      Sum     : Natural := 0;
   begin
      -- Read the input file.
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      while not Ada.Text_IO.End_Of_File (Input) loop
         declare
            Line    : constant String := Ada.Text_IO.Get_Line (Input);
            Matches : Match_Array (0 .. 2);
            Start   : Positive := Line'First;
            Left    : Integer;
            Right   : Integer;
         begin
            loop
               Match (Pattern, Line (Start .. Line'Last), Matches);

               -- If no more match is found, stop.
               exit when Matches (0) = No_Match;

               -- If there is a match, read left and right value and multiply them.
               Left  := Integer'Value (Line (Matches (1).First .. Matches (1).Last));
               Right := Integer'Value (Line (Matches (2).First .. Matches (2).Last));

               Sum := @ + (Left * Right);

               -- Set the next index to one after the last index of the current match.
               Start := Matches (0).Last + 1;
            end loop;
         end;
      end loop;

      Ada.Text_IO.Close (Input);

      return Sum;
   end Solution_Part1;

   function Solution_Part2 return Natural is
      Pattern : constant Pattern_Matcher := Compile ("do\(\)|don't\(\)|mul\(\-?(\d+),\-?(\d+)\)");
      Input   : Ada.Text_IO.File_Type;
      Sum     : Natural := 0;
      Enabled : Boolean  := True;
   begin
      -- Read the input file.
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      while not Ada.Text_IO.End_Of_File (Input) loop
         declare
            Line    : constant String := Ada.Text_IO.Get_Line (Input);
            Matches : Match_Array (0 .. 2);
            Start   : Positive := Line'First;
         begin
            loop
               Match (Pattern, Line (Start .. Line'Last), Matches);

               -- If no more match is found, stop.
               exit when Matches (0) = No_Match;

               -- If there is a match, check whether it's do, don't or mul.
               declare
                  Match : constant String := Line (Matches (0).First .. Matches (0).Last);
                  Left  : Integer;
                  Right : Integer;
               begin
                  -- Ada.Text_IO.Put_Line (Match);
                  if Match = "do()" then
                     Enabled := True;
                  elsif Match = "don't()" then
                     Enabled := False;
                  elsif Enabled then
                     Left  := Integer'Value (Line (Matches (1).First .. Matches (1).Last));
                     Right := Integer'Value (Line (Matches (2).First .. Matches (2).Last));

                     Sum := @ + (Left * Right);
                  end if;
               end;

               -- Set the next index to one after the last index of the current match.
               Start := Matches (0).Last + 1;
            end loop;
         end;
      end loop;

      Ada.Text_IO.Close (Input);

      return Sum;
   end Solution_Part2;

end AOC2024_03;