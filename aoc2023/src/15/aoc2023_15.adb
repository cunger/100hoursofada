with Ada.Text_IO;
with Util.Strings;

with Hash;
with Lens_Arrangement;

package body AOC2023_15 is

   function Read_Input return String;
   function Read_Input return String is
      Input : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);
      declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin
         Ada.Text_IO.Close (Input);
         return Line;
      end;
   end Read_Input;

   Input : constant String := Read_Input;

   -- Part 1
   function Hash_Sum_Of_Initialization_Sequence return Natural is
      Sum : Natural := 0;
   begin
      Process_Input : declare
         Prev_Comma : Natural := Input'First - 1;
         Next_Comma : Natural := Util.Strings.Index (Input, ',', From => Input'First);
      begin
         For_Each_Step : loop
            declare
               Step : constant String := Input ((Prev_Comma + 1) .. (Next_Comma - 1));
            begin
               exit For_Each_Step when Step = "";

               Sum := @ + Hash.Hash_256 (Step);

               Prev_Comma := Next_Comma;
               Next_Comma := Util.Strings.Index (Input, ',', From => Prev_Comma + 1);
               if Next_Comma = 0 then
                  Next_Comma := Input'Last + 1;
               end if;
            end;
         end loop For_Each_Step;
      end Process_Input;

      return Sum;
   end Hash_Sum_Of_Initialization_Sequence;

   -- Part 2
   function Total_Focusing_Power return Natural is
   begin
      Process_Input : declare
         Prev_Comma : Natural := Input'First - 1;
         Next_Comma : Natural := Util.Strings.Index (Input, ',', From => Input'First);
      begin
         For_Each_Step : loop
            declare
               Step : constant String := Input ((Prev_Comma + 1) .. (Next_Comma - 1));
            begin
               exit For_Each_Step when Step = "";

               Lens_Arrangement.Perform_Step (Step);

               Prev_Comma := Next_Comma;
               Next_Comma := Util.Strings.Index (Input, ',', From => Prev_Comma + 1);
               if Next_Comma = 0 then
                  Next_Comma := Input'Last + 1;
               end if;
            end;
         end loop For_Each_Step;

         return Lens_Arrangement.Focusing_Power;
      end Process_Input;
   end Total_Focusing_Power;

end AOC2023_15;