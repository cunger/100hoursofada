with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Day_03 is

   package IO renames Ada.Text_IO;

   type Digit is new Natural range 0 .. 9;

   function To_Joltage (D1 : Digit; D2 : Digit) return Natural;
   function To_Joltage (D1 : Digit; D2 : Digit) return Natural is
   begin
      return Natural'Value (
         Ada.Strings.Fixed.Trim (D1'Image, Ada.Strings.Left) &
         Ada.Strings.Fixed.Trim (D2'Image, Ada.Strings.Left)
      );
   end To_Joltage;

   function Sum_Joltages return Natural is
      Input : IO.File_Type;
      Joltages : Natural := 0;
   begin
      -- Open the input file in read mode.
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/03/input_03.txt");

      -- Walk through the file line by line.
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
            -- Current digit when waling through the line.
            D : Digit;
            -- Temporary variables for the two biggest digets.
            Left_Digit  : Digit := 0;
            Right_Digit : Digit := 0;
            -- Maximum Joltage that was achieved so far.
            Max_Joltage : Natural := 0;
         begin
            for I in Line'Range loop
               D := Digit'Value (Line (I .. I));

               -- If either of the digits is not yet set, then do so
               -- and continue to next one.
               if Left_Digit = 0 then
                  Left_Digit := D;
               elsif Right_Digit = 0 then
                  Right_Digit := D;
                  Max_Joltage := To_Joltage (Left_Digit, Right_Digit);
               else
                  -- Determine whether one combination increases Max_Joltage:
                  -- Left_Digit & D, or Right_Digit & D.
                  declare
                     Joltage1 : constant Natural := To_Joltage (Left_Digit, D);
                     Joltage2 : constant Natural := To_Joltage (Right_Digit, D);
                  begin
                     if Joltage1 > Max_Joltage and Joltage2 > Max_Joltage then
                        if Joltage1 >= Joltage2 then
                           Right_Digit := D;
                           Max_Joltage := Joltage1;
                        elsif Joltage2 > Joltage1 then
                           Left_Digit  := Right_Digit;
                           Right_Digit := D;
                           Max_Joltage := Joltage2;
                        end if;
                     elsif Joltage1 > Max_Joltage then
                        Right_Digit := D;
                        Max_Joltage := Joltage1;
                     elsif Joltage2 > Max_Joltage then
                        Left_Digit  := Right_Digit;
                        Right_Digit := D;
                        Max_Joltage := Joltage2;
                     end if;
                  end;
               end if;
            end loop;

            Joltages := @ + Max_Joltage;
         end;
      end loop;

      -- Finally, close the file again.
      IO.Close (Input);

      return Joltages;
   end Sum_Joltages;

   procedure Solve is
   begin
      IO.Put_Line ("Part 1: " & Sum_Joltages'Image);
      IO.Put_Line ("Part 2: -");
   end Solve;

end Day_03;