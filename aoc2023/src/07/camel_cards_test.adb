pragma Ada_2022;

with Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Camel_Cards; use Camel_Cards;

procedure Camel_Cards_Test is

   With_Joker : Boolean;

   procedure Check_Type_Of_Hand (Str : String; Expected : Hand_Type) is
      Actual : Hand_Type;
   begin
      Actual := Determine_Type (Parse_Hand (Str, With_Joker));
      Assert (
         Actual = Expected,
         Str & " is " & Actual'Image & " but should be " & Expected'Image
      );
   end Check_Type_Of_Hand;

   procedure Check_Most_Frequent_Card (Str : String; Expected : Card) is
      Parsed : Hand;
      Counts : Card_Counts := [others => 0];
      Actual : Card;
   begin
      Parsed := Parse_Hand (Str, With_Joker => True);
      
      for I in Hand'Range loop
         Counts (Parsed (I)) := @ + 1;
      end loop;

      Actual := Most_Frequent_Card (Counts);
      
      Assert (
         Actual = Expected,
         Str & " is " & Actual'Image & " but should be " & Expected'Image
      );
   end Check_Most_Frequent_Card;
begin
   Ada.Text_IO.Put_Line ("Running tests...");

   Ada.Text_IO.Put_Line ("Hand type without Joker...");
   With_Joker := False;

   Check_Type_Of_Hand ("AAAAA", Five_Of_A_Kind);
   Check_Type_Of_Hand ("222T2", Four_Of_A_Kind);
   Check_Type_Of_Hand ("23332", Full_House);
   Check_Type_Of_Hand ("2333J", Three_Of_A_Kind);
   Check_Type_Of_Hand ("23432", Two_Pair);
   Check_Type_Of_Hand ("A23A4", One_Pair);
   Check_Type_Of_Hand ("234TA", High_Card);

   Ada.Text_IO.Put_Line ("Hand type with Joker...");
   With_Joker := True;

   Check_Type_Of_Hand ("J22J2", Five_Of_A_Kind);
   Check_Type_Of_Hand ("JJJJJ", Five_Of_A_Kind);
   Check_Type_Of_Hand ("2JJJJ", Five_Of_A_Kind);
   Check_Type_Of_Hand ("2333J", Four_Of_A_Kind);
   Check_Type_Of_Hand ("677J7", Four_Of_A_Kind);
   Check_Type_Of_Hand ("KJ9JK", Four_Of_A_Kind);
   Check_Type_Of_Hand ("234J2", Three_Of_A_Kind);
   Check_Type_Of_Hand ("A23J4", One_Pair);

   Ada.Text_IO.Put_Line ("Most frequent card in a hand...");

   Check_Most_Frequent_Card ("KK234", King);
   Check_Most_Frequent_Card ("KQQQK", Queen);
   Check_Most_Frequent_Card ("2JJJJ", Two);
   Check_Most_Frequent_Card ("J3J3J", Three);
   Check_Most_Frequent_Card ("JJJJJ", Joker);

   Ada.Text_IO.Put_Line ("All fine.");
end Camel_Cards_Test;