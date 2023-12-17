pragma Ada_2022;

with Ada.Text_IO;
with Util.Strings;

package body AOC2023_15 is

   -- Part 1
   function Hash_Sum_Of_Initialization_Sequence return Natural is
      Input : Ada.Text_IO.File_Type;
      Sum   : Natural := 0;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      Process_Input : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
         Prev_Comma : Natural   := Line'First - 1;
         Next_Comma : Natural   := Util.Strings.Index (Line, ',', From => Line'First);
      begin
         For_Each_Step : loop
            declare
               Step : constant String := Line ((Prev_Comma + 1) .. (Next_Comma - 1));
            begin
               exit For_Each_Step when Step = "";

               Sum := @ + Hash_256 (Step);

               Prev_Comma := Next_Comma;
               Next_Comma := Util.Strings.Index (Line, ',', From => Prev_Comma + 1);
               if Next_Comma = 0 then
                  Next_Comma := Line'Last + 1;
               end if;
            end;
         end loop For_Each_Step;
      end Process_Input;

      Ada.Text_IO.Close (Input);

      return Sum;
   end Hash_Sum_Of_Initialization_Sequence;

   function Hash_256 (Str : String) return Hash_Value is
      Hash : Natural := 0;
   begin
      For_Each_Character : for Char of Str loop
         Hash := ((@ + Character'Pos (Char)) * 17) mod 256;
      end loop For_Each_Character;

      return Hash;
   end Hash_256;

end AOC2023_15;