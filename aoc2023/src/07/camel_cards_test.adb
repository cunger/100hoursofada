with Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Camel_Cards; use Camel_Cards;

procedure Camel_Cards_Test is

   procedure Check_Type_Of_Hand (Str : String; Expected_Type : Hand_Type) is
   begin
      Assert (
         Determine_Type (Parse_Hand (Str)) = Expected_Type,
         Str & " should be parsed as " & Expected_Type'Image
      );
   end Check_Type_Of_Hand;

begin
   Ada.Text_IO.Put_Line ("Running tests...");

   Check_Type_Of_Hand ("AAAAA", Five_Of_A_Kind);
   Check_Type_Of_Hand ("222T2", Four_Of_A_Kind);
   Check_Type_Of_Hand ("23332", Full_House);
   Check_Type_Of_Hand ("2333A", Three_Of_A_Kind);
   Check_Type_Of_Hand ("23432", Two_Pair);
   Check_Type_Of_Hand ("A23A4", One_Pair);
   Check_Type_Of_Hand ("234TA", High_Card);

   Ada.Text_IO.Put_Line ("Done.");
end Camel_Cards_Test;