pragma Ada_2022;

with Ada.Text_IO;
with Camel_Cards; use Camel_Cards;
package body AOC2023_07 is

   -- Part 1
   function Total_Winnings (With_Joker : Boolean := False) return Natural is
      use Hand_Info_Vectors;

      Input     : Ada.Text_IO.File_Type;
      All_Hands : Hand_Infos;
      Winnings  : Natural := 0;
   begin
      Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

      -- Parse all hands.
      while not Ada.Text_IO.End_Of_File (Input) loop
         declare
            Line     : constant String := Ada.Text_IO.Get_Line (Input);
            Cards    : Hand;
            Bid      : Natural;
            One_Hand : Hand_Info;
         begin
            Cards    := Parse_Hand (Line (Line'First .. Line'First + 4), With_Joker);
            Bid      := Integer'Value (Line (Line'First + 6 .. Line'Last));
            One_Hand := (Cards, Determine_Type (Cards), Bid);
            All_Hands.Append (One_Hand);
         end;
      end loop;

      Ada.Text_IO.Close (Input);

      -- Sort hands in increasing strength.
      Hand_Info_Sorting.Sort (All_Hands);

      -- Calculate total winnings.
      declare
         Position : Cursor := First (All_Hands);
      begin
         while Has_Element (Position) loop
            Winnings := @ + To_Index (Position) * Element (Position).Bid;
            Position := Next (Position);
         end loop;
      end;

      return Winnings;
   end Total_Winnings;

end AOC2023_07;