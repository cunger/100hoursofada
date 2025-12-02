with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Util.Strings;

package body Day_02 is

   package IO renames Ada.Text_IO;

   function Read_Input return String;
   function Read_Input return String is
      Input : IO.File_Type;
   begin
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/02/input_02.txt");
      declare
         Line : constant String := IO.Get_Line (Input);
      begin
         IO.Close (Input);
         return Line;
      end;
   end Read_Input;

   Input : constant String := Read_Input;

   -- Part 1
   function Sum_Invalid_Ids_1 return Big_Integer is
      Sum : Big_Integer := 0;
   begin
      Process_Input : declare
         Prev_Comma : Natural := Input'First - 1;
         Next_Comma : Natural := Util.Strings.Index (Input, ',', From => Input'First);
      begin
         For_Each_Range : loop
            declare
               This_Range : constant String := Input ((Prev_Comma + 1) .. (Next_Comma - 1));
            begin
               exit For_Each_Range when This_Range = "";

               declare
                  Index_Dash : constant Natural := Util.Strings.Index (This_Range, '-', From => This_Range'First);
                  Lower : constant Big_Integer  := From_String (This_Range (This_Range'First .. (Index_Dash - 1)));
                  Upper : constant Big_Integer  := From_String (This_Range ((Index_Dash + 1) .. This_Range'Last));
                  Id : Big_Integer := Lower;
               begin
                  while Id <= Upper loop
                     declare
                        Id_String : constant String := Ada.Strings.Fixed.Trim (Id'Image, Ada.Strings.Both);
                        Split_Index : constant Natural := Id_String'Length / 2;
                        Left : constant String := Id_String (Id_String'First .. Split_Index);
                        Right : constant String := Id_String (Split_Index + 1 .. Id_String'Last);
                     begin
                        if Left = Right then
                           Sum := @ + Id;
                        end if;
                        Id := @ + 1;
                     end;
                  end loop;

                  Prev_Comma := Next_Comma;
                  Next_Comma := Util.Strings.Index (Input, ',', From => Prev_Comma + 1);
                  if Next_Comma = 0 then
                     Next_Comma := Input'Last + 1;
                  end if;
               end;
            end;
         end loop For_Each_Range;
      end Process_Input;

      return Sum;
   end Sum_Invalid_Ids_1;

   -- Part 2
   function Sum_Invalid_Ids_2 return Big_Integer is
      Sum : Big_Integer := 0;
   begin
      Process_Input : declare
         Prev_Comma : Natural := Input'First - 1;
         Next_Comma : Natural := Util.Strings.Index (Input, ',', From => Input'First);
      begin
         For_Each_Range : loop
            declare
               This_Range : constant String := Input ((Prev_Comma + 1) .. (Next_Comma - 1));
            begin
               exit For_Each_Range when This_Range = "";

               declare
                  Index_Dash : constant Natural := Util.Strings.Index (This_Range, '-', From => This_Range'First);
                  Lower : constant Big_Integer  := From_String (This_Range (This_Range'First .. (Index_Dash - 1)));
                  Upper : constant Big_Integer  := From_String (This_Range ((Index_Dash + 1) .. This_Range'Last));
                  Id : Big_Integer := Lower;
               begin
                  while Id <= Upper loop
                     declare
                        Id_String : constant String  := Ada.Strings.Fixed.Trim (Id'Image, Ada.Strings.Both);
                        Id_Length : constant Natural := Id_String'Length;
                     begin
                        For_Each_Block_Length : for I in 1 .. Id_Length / 2 loop
                           declare
                              Block : constant String := Id_String (1 .. I);
                              Repeated_Block : constant Unbounded_String := (Id_Length / I) * To_Unbounded_String (Block);
                           begin
                              if Repeated_Block = Id_String then
                                 Sum := @ + Id;
                                 exit For_Each_Block_Length;
                              end if;
                           end;
                        end loop For_Each_Block_Length;
                        Id := @ + 1;
                     end;
                  end loop;

                  Prev_Comma := Next_Comma;
                  Next_Comma := Util.Strings.Index (Input, ',', From => Prev_Comma + 1);
                  if Next_Comma = 0 then
                     Next_Comma := Input'Last + 1;
                  end if;
               end;
            end;
         end loop For_Each_Range;
      end Process_Input;

      return Sum;
   end Sum_Invalid_Ids_2;

   procedure Solve is
   begin
      IO.Put_Line ("Part 1:" & Sum_Invalid_Ids_1'Image);
      IO.Put_Line ("Part 2:" & Sum_Invalid_Ids_2'Image);
   end Solve;

end Day_02;