with Ada.Text_IO;
with Ada.Strings.Fixed;
with Util.Strings;

package body AOC2023_02 is

   function All_Possible (Draws : String) return Boolean;

   function Parse_Draw (Str : String) return Draw;

   function Sum_IDs_Of_Possible_Games return Natural is
      Input : Ada.Text_IO.File_Type;
      Sum   : Natural := 0;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            Line        : constant String   := Ada.Text_IO.Get_Line (Input);
            Index_Colon : constant Positive := Util.Strings.Index (Line, ':', From => Line'First);
            Game_ID     : constant String   := Line (Line'First + 5 .. Index_Colon - 1);
            Draws       : constant String   := Line (Index_Colon + 2 .. Line'Last);
         begin
            if All_Possible (Draws) then
               Sum := Sum + Integer'Value (Game_ID);
            end if;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Sum;
   end Sum_IDs_Of_Possible_Games;

   function All_Possible (Draws : String) return Boolean is
      All_Possible : Boolean := True;

      -- Variables for looping over all draws
      Start_Index             : Natural := Draws'First;
      Index_Next_Semicolon : Natural;
      Parsed_Draw             : Draw;
   begin
      For_Each_Draw : loop
         exit For_Each_Draw when (Start_Index >= Draws'Last);

         Index_Next_Semicolon := Util.Strings.Index (Draws, ';', From => Start_Index);
         if Index_Next_Semicolon = 0 then
            Index_Next_Semicolon := Draws'Last + 1;
         end if;

         Parsed_Draw := Parse_Draw (Draws (Start_Index .. Index_Next_Semicolon - 1));
         Start_Index := Index_Next_Semicolon + 1;

         if Parsed_Draw.Red   > Max_Red  or
            Parsed_Draw.Blue  > Max_Blue or
            Parsed_Draw.Green > Max_Green
         then
            All_Possible := False;
         end if;

         exit For_Each_Draw when not All_Possible;
      end loop For_Each_Draw;

      return All_Possible;
   end All_Possible;

   function Parse_Draw (Str : String) return Draw is
      Reds, Blues, Greens : Natural := 0;

      -- Variables for looping over the colors
      Start_Index         : Natural := Str'First;
      Index_Next_Comma : Natural;
   begin
      For_Each_Color_Info : loop
         exit For_Each_Color_Info when (Start_Index >= Str'Last);

         Index_Next_Comma := Util.Strings.Index (Str, ',', From => Start_Index);
         if Index_Next_Comma = 0 then
            Index_Next_Comma := Str'Last + 1;
         end if;

         Parse_Color : declare
            Color_Info : constant String := Ada.Strings.Fixed.Trim (
               Str (Start_Index .. Index_Next_Comma - 1),
               Ada.Strings.Left
            );
            Index_Space : constant Natural := Util.Strings.Index (Color_Info, ' ');
         begin
            if Util.Strings.Ends_With (Color_Info, "red") then
               Reds := Integer'Value (Color_Info (Color_Info'First .. Index_Space - 1));
            end if;

            if Util.Strings.Ends_With (Color_Info, "blue") then
               Blues := Integer'Value (Color_Info (Color_Info'First .. Index_Space - 1));
            end if;

            if Util.Strings.Ends_With (Color_Info, "green") then
               Greens := Integer'Value (Color_Info (Color_Info'First .. Index_Space - 1));
            end if;
         end Parse_Color;

         Start_Index := Index_Next_Comma + 1;
      end loop For_Each_Color_Info;

      return (Red => Reds, Blue => Blues, Green => Greens);
   end Parse_Draw;

end AOC2023_02;