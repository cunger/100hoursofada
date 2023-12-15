pragma Ada_2022;

package body Camel_Cards is

   function Parse_Card (Letter : in Character; With_Joker : Boolean := False) return Card is
   begin
      return (case Letter is
         when '2' => Two,
         when '3' => Three,
         when '4' => Four,
         when '5' => Five,
         when '6' => Six,
         when '7' => Seven,
         when '8' => Eight,
         when '9' => Nine,
         when 'T' => Ten,
         when 'J' => (if With_Joker then Joker else Jack),
         when 'Q' => Queen,
         when 'K' => King,
         when 'A' => Ace,
         when others => raise Invalid_Input with "Not a valid card: " & Letter
      );
   end Parse_Card;

   function Parse_Hand (Str : String; With_Joker : Boolean := False) return Hand is
      Parsed : Hand;
   begin
      for I in 1 .. 5 loop
         Parsed (I) := Parse_Card (Str (Str'First + (I - 1)), With_Joker);
      end loop;

      return Parsed;
   end Parse_Hand;

   function Most_Frequent_Card (Counts : in Card_Counts) return Card is
      Winner : Card := Joker;
   begin
      for C in Card_Counts'Range loop
         if not (C = Joker) and
            (Counts (C) > 0) and
            ((Counts (C) > Counts (Winner)) or Winner = Joker)
         then
            Winner := C;
         end if;
      end loop;

      return Winner;
   end Most_Frequent_Card;

   function Determine_Type (Cards : in Hand) return Hand_Type is
      Counts : Card_Counts := [others => 0];
      Card_With_Max_Counts : Card;
   begin
      -- Count how often each card occurs in the hand.
      for I in Hand'Range loop
         Counts (Cards (I)) := @ + 1;
      end loop;

      -- Use Jokers to increase strength of the hand:
      -- Increase the count of the most frequent non-Joker card
      -- by the amount of Jokers.
      Card_With_Max_Counts := Most_Frequent_Card (Counts);
      if not (Card_With_Max_Counts = Joker) then
         Counts (Card_With_Max_Counts) := @ + Counts (Joker);
         Counts (Joker) := 0;
      end if;

      -- If a card occurs five times, then we have five of a kind.
      if (for some C of Counts => C = 5) then
         return Five_Of_A_Kind;
      end if;

      -- If a card occurs four times, then we have four of a kind.
      if (for some C of Counts => C = 4) then
         return Four_Of_A_Kind;
      end if;

      -- If a card occurs three times, then we have either a full house
      -- (if the other two cards are equal) or three of a kind.
      if (for some C of Counts => C = 3) then
         if (for all C of Counts => C = 0 or C >= 2) then
            return Full_House;
         else
            return Three_Of_A_Kind;
         end if;
      end if;

      -- Otherwise we have either two pairs, one, or none.
      Count_Pairs : declare
         Number_Of_Pairs : Natural := 0;
      begin
         for C of Counts loop
            if C = 2 then
               Number_Of_Pairs := @ + 1;
            end if;
         end loop;

         case Number_Of_Pairs is
            when 0      => return High_Card;
            when 1      => return One_Pair;
            when 2      => return Two_Pair;
            when others => return Unknown; -- This should never happen.
         end case;
      end Count_Pairs;
   end Determine_Type;

   function "<" (Left, Right : in Hand_Info) return Boolean is
   begin
      -- First check the type of the hand (using the ordering in the enum definition).
      if Left.Kind < Right.Kind then
         return True;
      elsif Left.Kind > Right.Kind then
         return False;
      end if;

      -- If the type is equal, check the cards one by one.
      for I in Left.Cards'Range loop
         if Left.Cards (I) < Right.Cards (I) then
            return True;
         elsif Left.Cards (I) > Right.Cards (I) then
            return False;
         end if;
      end loop;

      -- If we haven't returned yet, then the hands have equal strength.
      return False;
   end "<";

end Camel_Cards;