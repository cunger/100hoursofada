with Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;

procedure Day_02 is

   package IO renames Ada.Text_IO;

   Input : IO.File_Type;

   Solution_Part1 : Natural := 0;
   Solution_Part2 : Natural := 0;

   Pattern : constant Pattern_Matcher := Compile ("(\d+)x(\d+)x(\d+)");

   function Surface (Length, Width, Height : Natural) return Natural is
   begin
      return 2 * Length * Width + 2 * Width * Height + 2 * Height * Length;
   end Surface;

   function Smallest_Surface (Length, Width, Height : Natural) return Natural is
   begin
      return Natural'Min (Length * Width, Natural'Min (Width * Height, Height * Length));
   end Smallest_Surface;

   function Shortest_Distance (Length, Width, Height : Natural) return Natural is
   begin
      if Length >= Width and Length >= Height then
         return 2 * Width + 2 * Height;
      end if;
      if Width >= Length and Width >= Height then
         return 2 * Length + 2 * Height;
      end if;
      if Height >= Length and Height >= Width then
         return 2 * Length + 2 * Width;
      end if;
   end Shortest_Distance;

   function Volume (Length, Width, Height : Natural) return Natural is
   begin
      return Length * Width * Height;
   end Volume;
begin
   -- Open the input file in read mode.
   IO.Open (File => Input, Mode => IO.In_File, Name => "input/input_02.txt");

   -- Walk through the file line by line.
   while not IO.End_Of_File (Input) loop
      declare
         Line    : constant String := IO.Get_Line (Input);
         Matches : Match_Array (0 .. 3);
         Length  : Natural;
         Width   : Natural;
         Height  : Natural;
      begin
         Match (Pattern, Line, Matches);
         if Matches (0) /= No_Match then
            Length := Natural'Value (Line (Matches (1).First .. Matches (1).Last));
            Width  := Natural'Value (Line (Matches (2).First .. Matches (2).Last));
            Height := Natural'Value (Line (Matches (3).First .. Matches (3).Last));

            Solution_Part1 := @ + Surface (Length, Width, Height);
            Solution_Part1 := @ + Smallest_Surface (Length, Width, Height);

            Solution_Part2 := @ + Shortest_Distance (Length, Width, Height);
            Solution_Part2 := @ + Volume (Length, Width, Height);
         end if;
      end;
   end loop;

   -- Finally, close the file again.
   IO.Close (Input);

   IO.Put_Line ("Day 2 Part 1:" & Natural'Image (Solution_Part1));
   IO.Put_Line ("Day 2 Part 2:" & Natural'Image (Solution_Part2));

end Day_02;