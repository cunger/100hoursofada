with Ada.Text_IO;
with Util.Strings;

package body Day_05 is

   package IO renames Ada.Text_IO;

   Fresh_Ingredients : Ingredient_Ranges;
   Avail_Ingredients : Ingredient_Ids;

   procedure Read_Input;
   procedure Read_Input is
      Input : IO.File_Type;
      Part1 : Boolean := True;
      -- This keeps track of in which part of the file we are.
      -- Is set to False once we get to the second part of the input file.
      Fresh_Index : Positive := 1;
      Avail_Index : Positive := 1;
   begin
      -- Open the input file in read mode.
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/05/input_05.txt");
      -- Walk through the file line by line.
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            if Line = "" then
               -- Skip line and mark transition to second part.
               Part1 := False;
            elsif Part1 then
               -- Read fresh ingredient range.
               declare
                  Index_Dash : constant Natural     := Util.Strings.Index (Line, '-', From => Line'First);
                  Start_Id   : constant Big_Integer := From_String (Line (Line'First .. (Index_Dash - 1)));
                  End_Id     : constant Big_Integer := From_String (Line ((Index_Dash + 1) .. Line'Last));
               begin
                  Fresh_Ingredients (Fresh_Index) := (Lower_Id => Start_Id, Upper_Id => End_Id);
                  Fresh_Index := @ + 1;
               end;
            else
               -- Read available ingredient id.
               Avail_Ingredients (Avail_Index) := From_String (Line);
               Avail_Index := @ + 1;
            end if;
         end;
      end loop;

      IO.Close (Input);
   end Read_Input;

   function Number_Of_Available_Fresh_Ingredients return Natural;
   function Number_Of_Available_Fresh_Ingredients return Natural is
      Count : Natural := 0;
   begin
      -- For each available ingredient, check whether it's in any range of fresh ingredients.
      for I in Avail_Ingredients'Range loop
         declare
            Found : Boolean := False;
         begin
            for J in Fresh_Ingredients'Range loop
               if Avail_Ingredients (I) >= Fresh_Ingredients (J).Lower_Id and
                  Avail_Ingredients (I) <= Fresh_Ingredients (J).Upper_Id
               then
                  Count := @ + 1;
                  Found := True;
               end if;
               -- Make sure to not count ingredients more than once,
               -- just because they occur in several ranges.
               exit when Found;
            end loop;
         end;
      end loop;

      return Count;
   end Number_Of_Available_Fresh_Ingredients;

   function Number_Of_Fresh_Ingredients return Big_Integer;
   function Number_Of_Fresh_Ingredients return Big_Integer is
      Count : Big_Integer := 0;
   begin
      -- TODO
      -- (Max Upper - Min Lower) - gaps

      return Count;
   end Number_Of_Fresh_Ingredients;

   procedure Solve is
   begin
      Read_Input;
      IO.Put_Line ("Part 1:" & Number_Of_Available_Fresh_Ingredients'Image);
      IO.Put_Line ("Part 2:" & Number_Of_Fresh_Ingredients'Image);
   end Solve;

end Day_05;