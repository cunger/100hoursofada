pragma Ada_2022;

with Ada.Text_IO;
with Util.Strings;

package body AOC2023_04 is

   -- For storing the winning numbers, use an array that specifies
   -- for each number in the range whether it appears on the card
   -- as winning number or not. (Idea stolen from John Perry.)
   type Number  is range 0 .. 99;
   type Numbers is array (Number) of Boolean;

   package Num_IO is new Ada.Text_IO.Integer_IO (Number);

   -- Sum the points that the scratch cards in the inut are worth.
   function Points_Of_Scratch_Cards return Natural is
      Input  : Ada.Text_IO.File_Type;
      Points : Natural := 0;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      while not Ada.Text_IO.End_Of_File (Input) loop
         Process_Line : declare
            -- The current line
            Line : constant String := Ada.Text_IO.Get_Line (Input);

            -- The relevant parts of the current line
            Index_Colon : constant Positive := Util.Strings.Index (Line, ':', From => Line'First);
            Index_Pipe  : constant Positive := Util.Strings.Index (Line, '|', From => Line'First);
            Left_Part   : constant String   := Line (Index_Colon + 2 .. Index_Pipe - 2);
            Right_Part  : constant String   := Line (Index_Pipe + 2 .. Line'Last);

            -- Initialize winning numbers and count of matching numbers
            Winning_Numbers  : Numbers := [others => False];
            Matching_Numbers : Natural := 0;
         begin
            Parse_Winning_Numbers :
            declare
               Index : Natural := Left_Part'First;
               Value : Number;
            begin
               for I in 1 .. 10 loop
                  Num_IO.Get (Left_Part (Index .. Index + 2), Value, Index);
                  Index := @ + 1;

                  Winning_Numbers (Value) := True;
               end loop;
            end Parse_Winning_Numbers;

            Count_Matching_Numbers :
            declare
               Index : Natural := Right_Part'First;
               Value : Number;
            begin
               for I in 1 .. 25 loop
                  Num_IO.Get (Right_Part (Index .. Index + 2), Value, Index);
                  Index := @ + 1;

                  if Winning_Numbers (Value) then
                     Matching_Numbers := @ + 1;
                  end if;
               end loop;
            end Count_Matching_Numbers;

            if Matching_Numbers > 0 then
               Points := @ + 2**(Matching_Numbers - 1);
            end if;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Points;
   end Points_Of_Scratch_Cards;

end AOC2023_04;