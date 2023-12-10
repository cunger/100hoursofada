with Ada.Text_IO;
with Util.Strings;

package body AOC2023_01 is

   function First_Digit (Line : String; With_Words : Boolean) return Character;
   function Last_Digit  (Line : String; With_Words : Boolean) return Character;

   function Sum_Of_Calibration_Values (With_Words : Boolean) return Natural is
      Input : Ada.Text_IO.File_Type;
      Sum   : Natural := 0;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line  : constant String := Ada.Text_IO.Get_Line (Input);
            Value : Natural;
         begin
            Value := Integer'Value (First_Digit (Line, With_Words) & Last_Digit (Line, With_Words));
            Sum   := Sum + Value;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Sum;
   end Sum_Of_Calibration_Values;

   function First_Digit (Line : String; With_Words : Boolean) return Character is
   begin
      For_Each_Start_Index : for Index in 1 .. Line'Length loop
         declare
            Char : constant Character := Line (Index);
            Tail : constant String    := Line (Index .. Line'Last);
         begin
            if Char in '0' .. '9' then return Char; end if;
            if With_Words then
               if Util.Strings.Starts_With (Tail, "zero")  then return '0'; end if;
               if Util.Strings.Starts_With (Tail, "one")   then return '1'; end if;
               if Util.Strings.Starts_With (Tail, "two")   then return '2'; end if;
               if Util.Strings.Starts_With (Tail, "three") then return '3'; end if;
               if Util.Strings.Starts_With (Tail, "four")  then return '4'; end if;
               if Util.Strings.Starts_With (Tail, "five")  then return '5'; end if;
               if Util.Strings.Starts_With (Tail, "six")   then return '6'; end if;
               if Util.Strings.Starts_With (Tail, "seven") then return '7'; end if;
               if Util.Strings.Starts_With (Tail, "eight") then return '8'; end if;
               if Util.Strings.Starts_With (Tail, "nine")  then return '9'; end if;
            end if;
         end;
      end loop For_Each_Start_Index;

      return '0';
   end First_Digit;

   function Last_Digit (Line : String; With_Words : Boolean) return Character is
   begin
      For_Each_End_Index : for Index in reverse 1 .. Line'Length loop
         declare
            Char : constant Character := Line (Index);
            Head : constant String    := Line (Line'First .. Index);
         begin
            if Char in '0' .. '9' then return Char; end if;
            if With_Words then
               if Util.Strings.Ends_With (Head, "zero")  then return '0'; end if;
               if Util.Strings.Ends_With (Head, "one")   then return '1'; end if;
               if Util.Strings.Ends_With (Head, "two")   then return '2'; end if;
               if Util.Strings.Ends_With (Head, "three") then return '3'; end if;
               if Util.Strings.Ends_With (Head, "four")  then return '4'; end if;
               if Util.Strings.Ends_With (Head, "five")  then return '5'; end if;
               if Util.Strings.Ends_With (Head, "six")   then return '6'; end if;
               if Util.Strings.Ends_With (Head, "seven") then return '7'; end if;
               if Util.Strings.Ends_With (Head, "eight") then return '8'; end if;
               if Util.Strings.Ends_With (Head, "nine")  then return '9'; end if;
            end if;
         end;
      end loop For_Each_End_Index;

      return '0';
   end Last_Digit;

end AOC2023_01;