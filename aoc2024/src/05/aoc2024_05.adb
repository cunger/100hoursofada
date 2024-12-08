with AOC2024_05_Input; use AOC2024_05_Input;

package body AOC2024_05 with SPARK_Mode => Off is
   use Natural_Vectors;

   Data : constant Input_Data := Parse_Input_Data (Input_File_Name);

   function Obeys_Ordering_Rules (Update : Vector; Rules : Order_Rules) return Boolean;

   function Middle_Page_Number (Update : Vector) return Natural;

   function Solution_Part1 return Natural is
      Sum : Natural := 0;
   begin
      for Update of Data.Updates when Obeys_Ordering_Rules (Update, Data.Rules) loop
         Sum := @ + Middle_Page_Number (Update);
      end loop;
      return Sum;
   end Solution_Part1;

   function Solution_Part2 return Natural is
   begin
      return 0;
   end Solution_Part2;

   function Obeys_Ordering_Rules (Update : Vector; Rules : Order_Rules) return Boolean is
   begin
      for Rule of Rules loop
         declare
            Index1 : constant Extended_Index := Update.Find_Index (Rule.First);
            Index2 : constant Extended_Index := Update.Find_Index (Rule.Second);
         begin
            if Index1 /= No_Index and Index2 /= No_Index and Index1 > Index2 then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Obeys_Ordering_Rules;

   function Middle_Page_Number (Update : Vector) return Natural is
   begin
      return Element (Update, (Last_Index (Update) + 1) / 2);
   end Middle_Page_Number;

end AOC2024_05;