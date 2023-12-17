with Ada.Containers.Vectors;

package Camel_Cards is

   -----------
   -- Types --
   -----------

   type Card is (
      Joker,
      Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
      Jack, Queen, King, Ace
   );

   -- A hand consists of five cards (ordered)
   type Hand is array (1 .. 5) of Card;

   -- Each hand has a certain type.
   -- See camel_cards-test.adb for examples.
   type Hand_Type is (
      Unknown,
      High_Card,
      One_Pair,
      Two_Pair,
      Three_Of_A_Kind,
      Full_House,
      Four_Of_A_Kind,
      Five_Of_A_Kind
   );

   -- Info about a hand: the cards it holds, its type, and the amount that was bid.
   type Hand_Info is record
      Cards : Hand;
      Kind  : Hand_Type;
      Bid   : Natural;
   end record;

   subtype Rank is Positive range 1 .. 1000;

   package Hand_Info_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Rank,
      Element_Type => Hand_Info
   );

   subtype Hand_Infos is Hand_Info_Vectors.Vector;

   -------------------
   -- Functionality --
   -------------------

   function Parse_Hand (Str : in String; With_Joker : Boolean := False) return Hand
      with Pre => Str'Length = 5;
   -- Parses a string as a hand of cards. The provided Boolean
   -- determines whether 'J' is parsed as Jack or as Joker.
   -- Example: "A24QQ" is parsed as [Ace, Two, Four, Queen, Queen]
   -- Raises:  Invalid_Input exception

   function Determine_Type (Cards : in Hand) return Hand_Type
      with Post => not (Determine_Type'Result = Unknown);

   function "<" (Left, Right : in Hand_Info) return Boolean;
   -- Comparison of hand infos based on their strength.
   -- Used to determine order for sorting.

   package Hand_Info_Sorting is new Hand_Info_Vectors.Generic_Sorting;

   ----------------
   -- Exceptions --
   ----------------

   Invalid_Input : exception;

private

   function Parse_Card (Letter : in Character; With_Joker : Boolean := False) return Card;
   -- Parses a letter as a card. The provided Boolean
   -- determines whether 'J' is parsed as Jack or as Joker.

   type Card_Counts is array (Card) of Natural range 0 .. 5;

   function Most_Frequent_Card (Counts : Card_Counts) return Card;
   -- Finds the card that occurs most often in a hand and is not a Joker.
   -- (Unless all cards are Jokers, then it does return Joker.)
   -- If there's a tie, it doesn't matter which of the cards is returned.

end Camel_Cards;