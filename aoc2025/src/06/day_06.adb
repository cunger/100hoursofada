with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

package body Day_06 is
   use Ada.Strings;

   package IO renames Ada.Text_IO;

   function Parse_Num (Str : String) return Integer;
   function Parse_Num (Str : String) return Integer is
   begin
      return Integer'Value (Fixed.Trim (Str, Both));
   end Parse_Num;

   function Parse_Op (Str : String) return Operation;
   function Parse_Op (Str : String) return Operation is
      Trimmed_Str : constant String := Fixed.Trim (Str, Both);
   begin
      if Trimmed_Str = "+" then
         return Sum;
      elsif Trimmed_Str = "*" then
         return Multiplication;
      else
         return None;
      end if;
   end Parse_Op;

   function Read_Input_Part1 return Homework_Tasks;
   function Read_Input_Part1 return Homework_Tasks is
      Input : IO.File_Type;
      Homework : Homework_Tasks := [others => (0, 0, 0, 0, None)];
      Row : Positive := 1;
   begin
      -- Open the input file in read mode.
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/06/input_06.txt");
      -- Walk through the file line by line.
      while not IO.End_Of_File (Input) loop
         declare
            Line   : constant String := IO.Get_Line (Input);
            Start  : Positive := Line'First;
            Column : Positive := 1;
            First  : Positive;
            Last   : Natural;
         begin
            -- Iterate over all whitespace-separated substrings.
            loop
               Fixed.Find_Token (
                  Source => Line,
                  Set    => Maps.To_Set (' '),
                  From   => Start,
                  Test   => Outside,
                  First  => First,
                  Last   => Last
               );
               exit when Last = 0;

               case Row is
                  when 1 => Homework (Column).N1 := Parse_Num (Line (First .. Last));
                  when 2 => Homework (Column).N2 := Parse_Num (Line (First .. Last));
                  when 3 => Homework (Column).N3 := Parse_Num (Line (First .. Last));
                  when 4 => Homework (Column).N4 := Parse_Num (Line (First .. Last));
                  when 5 => Homework (Column).Op := Parse_Op  (Line (First .. Last));
                  when others => null;
               end case;

               Start := Last + 1;
               exit when Start > Line'Last;

               Column := @ + 1;
            end loop;
         end;

         Row := @ + 1;
      end loop;

      IO.Close (Input);

      return Homework;
   end Read_Input_Part1;

   function Read_Input_Part2 return Homework_Tasks;
   function Read_Input_Part2 return Homework_Tasks is
      Homework : Homework_Tasks;
      Input    : IO.File_Type;
   begin
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/06/input_06.txt");
      -- Read all five lines of the input file.
      declare 
         Line1 : constant String := IO.Get_Line (Input);
         Line2 : constant String := IO.Get_Line (Input);
         Line3 : constant String := IO.Get_Line (Input);
         Line4 : constant String := IO.Get_Line (Input);
         Line5 : constant String := IO.Get_Line (Input);

         H_Index : Positive := 1;
         N_Index : Positive := 1;
      begin
         for I in Line1'Range loop
            declare
               C1 : String := Line1 (I .. I);
               C2 : String := Line2 (I .. I);
               C3 : String := Line3 (I .. I);
               C4 : String := Line4 (I .. I);
               C5 : String := Line5 (I .. I);
            begin
               if C1 = " " and C2 = " " and C3 = " " and C4 = " " and C5 = " " then
                  -- If the column is empty, then go to the next homework task.
                  H_Index := @ + 1;
                  N_Index := 1;
               else
                  -- If the operation is given in this column, reset homework
                  -- with the operation and its neutral element.
                  if C5 = "+" then
                     Homework (H_Index) := (0, 0, 0, 0, Sum);
                  elsif C5 = "*" then
                     Homework (H_Index) := (1, 1, 1, 1, Multiplication);
                  end if;
                  -- Read the next number.
                  if N_Index = 1 then
                     Homework (H_Index).N1 := Parse_Num (C1 & C2 & C3 & C4);
                  elsif N_Index = 2 then
                     Homework (H_Index).N2 := Parse_Num (C1 & C2 & C3 & C4);
                  elsif N_Index = 3 then
                     Homework (H_Index).N3 := Parse_Num (C1 & C2 & C3 & C4);
                  elsif N_Index = 4 then
                     Homework (H_Index).N4 := Parse_Num (C1 & C2 & C3 & C4);
                  end if;
                  -- Move to next column.
                  N_Index := @ + 1;
               end if;
            end;
         end loop;
      end;
      return Homework;
   end Read_Input_Part2;

   function Do_Homework (Homework : Homework_Tasks) return Big_Integer;
   function Do_Homework (Homework : Homework_Tasks) return Big_Integer is
      Result : Big_Integer := 0;
   begin
      for H of Homework loop
         if H.Op = Sum then
            Result := @ + 
               To_Big_Integer (H.N1) +
               To_Big_Integer (H.N2) +
               To_Big_Integer (H.N3) +
               To_Big_Integer (H.N4);
         elsif H.Op = Multiplication then
            Result := @ +
               To_Big_Integer (H.N1) *
               To_Big_Integer (H.N2) * 
               To_Big_Integer (H.N3) * 
               To_Big_Integer (H.N4);
         end if;
      end loop;

      return Result;
   end Do_Homework;

   procedure Solve is
   begin
      IO.Put_Line ("Part 1: " & To_String (Do_Homework (Read_Input_Part1)));
      IO.Put_Line ("Part 2: " & To_String (Do_Homework (Read_Input_Part2)));
   end Solve;

end Day_06;