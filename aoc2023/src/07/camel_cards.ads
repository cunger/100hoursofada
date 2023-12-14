with Ada.Containers.Vectors;

package Camel_Cards is

   -----------
   -- Types --
   -----------

   type Card is (Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace);

   -- A hand consists of five cards (ordered)
   type Hand is array (1 .. 5) of Card;

   -- Each hand has a certain type.
   -- See camel_cards_test.adb for examples.
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

   function Parse_Card (Letter : in Character) return Card;

   -- Parses a string as a hand of cards.
   -- Example: "A24JJ" is parsed as [Ace, Two, Four, Jack, Jack]
   -- Raises:  Invalid_Input exception
   function Parse_Hand (Str : in String) return Hand
      with Pre => Str'Length = 5;

   function Determine_Type (Cards : in Hand) return Hand_Type
      with Post => not (Determine_Type'Result = Unknown);

   -- Comparison of hand infos based on their strength.
   -- Used to determine order in Sort_By_Increasing_Strength.
   function "<" (Left, Right : in Hand_Info) return Boolean;

   package Hand_Info_Sorting is new Hand_Info_Vectors.Generic_Sorting;

   ----------------
   -- Exceptions --
   ----------------

   Invalid_Input : exception;

end Camel_Cards;