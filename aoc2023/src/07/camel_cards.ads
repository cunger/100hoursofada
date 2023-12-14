package Camel_Cards is

   Invalid_Input : exception;

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

   -- Info about a hand: the cards it holds, its type, and the amount that was bet.
   type Hand_Info is record
      Cards : Hand;
      Kind  : Hand_Type;
      Bid   : Natural;
   end record;

   type Hand_Infos is array (1 .. 1000) of Hand_Info;

   -- Functionality

   -- Parses a string as a hand of cards.
   -- Example: "A24JJ" is parsed as (Ace, Two, Four, Jack, Jack)
   -- Raises:  Invalid_Input exception
   function Parse_Hand (Str : String) return Hand
      with Pre => Str'Length = 5;

   function Determine_Type (Cards : in Hand) return Hand_Type
      with Post => not (Determine_Type'Result = Unknown);

   procedure Sort_By_Increasing_Strength (Inputs : in out Hand_Infos);

end Camel_Cards;