pragma Ada_2022;

with Ada.Text_IO;
with Util.Strings;

package body AOC2023_04 is

   -- For storing the winning numbers of a card, use an array that specifies
   -- for each number in the range whether it appears on the card as winning
   -- number or not. (Idea stolen from John Perry.)
   type Number  is range 0 .. 99;
   type Numbers is array (Number) of Boolean;

   package Num_IO is new Ada.Text_IO.Integer_IO (Number);

   -- For storing how many matching numbers each card has.
   type Card_Pile is array (1 .. 211) of Natural;

   -- Process the cards in the input file and store the number of matches for each card.
   function Process_Scratch_Cards return Card_Pile;

   -- Count the total number of copies that a specific card wins.
   function Total_Number_Of_Copies (Card_Number : Positive; Cards : Card_Pile) return Natural;

   -- Part 1: Sum the points that the scratch cards in the input are worth.
   function Total_Number_Of_Points return Natural is
      Points : Natural := 0;
      Cards  : Card_Pile;
   begin
      Cards := Process_Scratch_Cards;

      for Card_Number in Cards'Range loop
         declare
            Matching_Numbers : constant Natural := Cards (Card_Number);
         begin
            if Matching_Numbers > 0 then
               Points := @ + 2**(Matching_Numbers - 1);
            end if;
         end;
      end loop;

      return Points;
   end Total_Number_Of_Points;

   -- Part 2: Count the total number of scratch cards you have in the end,
   -- both original and won.
   function Total_Number_Of_Cards return Natural is
      Total : Natural;
      Cards : Card_Pile;
   begin
      Cards := Process_Scratch_Cards;
      Total := Cards'Length;

      for Card_Number in Cards'Range loop
         Total := @ + Total_Number_Of_Copies (Card_Number, Cards);
      end loop;

      return Total;
   end Total_Number_Of_Cards;

   function Total_Number_Of_Copies (Card_Number : Positive; Cards : Card_Pile) return Natural is
      Matches : Natural := Cards (Card_Number);
      Next_Start, Next_End : Positive;
   begin
      if Matches > 0 and Card_Number < Cards'Last then
         Next_Start := Card_Number + 1;
         Next_End   := Integer'Min (Card_Number + Matches, Cards'Last);

         for Next in Next_Start .. Next_End loop
            Matches := @ + Total_Number_Of_Copies (Next, Cards);
         end loop;
      end if;

      return Matches;
   end Total_Number_Of_Copies;

   function Process_Scratch_Cards return Card_Pile is
      Input       : Ada.Text_IO.File_Type;
      Cards       : Card_Pile := [others => 0];
      Card_Number : Positive  := 1;
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

            Cards (Card_Number) := Matching_Numbers;
            Card_Number := @ + 1;
         end Process_Line;
      end loop;

      Ada.Text_IO.Close (Input);

      return Cards;
   end Process_Scratch_Cards;

end AOC2023_04;