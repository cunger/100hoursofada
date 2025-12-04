with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Day_03 is

   package IO renames Ada.Text_IO;

   function Find_Largest_Battery (Bank : Battery_Bank; Start_Index : Natural; End_Index : Natural) return Battery is
      Max_Battery : Battery;
   begin
      for I in Start_Index .. End_Index loop
         if I = Start_Index then
            Max_Battery := Bank (I);
         elsif Bank (I).Value > Max_Battery.Value then
            Max_Battery := Bank (I);
         end if;
      end loop;

      return Max_Battery;
   end Find_Largest_Battery;

   function Sum_Joltages (Number_Of_Batteries : Natural) return Big_Integer is
      Input    : IO.File_Type;
      Joltages : Big_Integer := 0;
   begin
      -- Open the input file in read mode.
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/03/input_03.txt");

      -- Walk through the file line by line.
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
            Bank : Battery_Bank;
            -- The battery configuration with largest joltage in that line.
            Batteries : String (1 .. Number_Of_Batteries);
         begin
            -- Read line into a battery bank.
            for I in Line'Range loop
               Bank (I) := (I, Digit'Value (Line (I .. I)));
            end loop;

            -- Pick the largest next battery (leaving enough space at the end of the bank to find as many more as needed).
            declare
               Prev_Index : Natural := 0;
               Next_Battery : Battery;
            begin
               for I in 1 .. Number_Of_Batteries loop
                  Next_Battery  := Find_Largest_Battery (Bank, Prev_Index + 1, Line'Last - Number_Of_Batteries + I);
                  Batteries (I) := Ada.Strings.Fixed.Trim (Next_Battery.Value'Image, Ada.Strings.Left) (1);
                  Prev_Index    := Next_Battery.Index;
               end loop;
            end;

            Joltages := @ + From_String (Batteries);
         end;
      end loop;

      -- Finally, close the file again.
      IO.Close (Input);

      return Joltages;
   end Sum_Joltages;

   procedure Solve is
   begin
      IO.Put_Line ("Part 1: " & Sum_Joltages (2)'Image);
      IO.Put_Line ("Part 2: " & Sum_Joltages (12)'Image);
   end Solve;

end Day_03;