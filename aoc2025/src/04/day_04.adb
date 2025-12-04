with Ada.Text_IO;

package body Day_04 is

   package IO renames Ada.Text_IO;

   function Read_Input return Grid;
   function Read_Input return Grid is
      Input  : IO.File_Type;
      Output : Grid := [others => [others => 0]];
      Row    : Natural := 1;
   begin
      -- Open the input file in read mode.
      IO.Open (File => Input, Mode => IO.In_File, Name => "src/04/input_04.txt");
      -- Walk through the file line by line.
      while not IO.End_Of_File (Input) loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for I in Line'Range loop
               if Line (I) = '@' then
                  Output (Row, I) := 1;
               end if;
            end loop;
            Row := @ + 1;
         end;
      end loop;

      IO.Close (Input);

      return Output;
   end Read_Input;

   Floor : Grid := Read_Input;

   function Number_Of_Accessible_Rolls (G : in out Grid; Remove : Boolean) return Natural is
      Count : Natural := 0;
      Adjacent_Rolls : Natural;
   begin
      for Row in 1 .. 139 loop
         for Col in 1 .. 140 loop
            Adjacent_Rolls := G (Row - 1, Col) + G (Row + 1, Col) +
               G (Row - 1, Col - 1) + G (Row, Col - 1) + G (Row + 1, Col - 1) +
               G (Row - 1, Col + 1) + G (Row, Col + 1) + G (Row + 1, Col + 1);
            if G (Row, Col) = 1 and Adjacent_Rolls < 4 then
               Count := @ + 1;
               if Remove then
                  G (Row, Col) := 0;
               end if;
            end if;
         end loop;
      end loop;
      return Count;
   end Number_Of_Accessible_Rolls;

   function Number_Of_Recursively_Accessible_Rolls (G : in out Grid) return Natural is
      Total : Natural := 0;
      Count : Natural;
   begin
      loop
         Count := Number_Of_Accessible_Rolls (G, True);
         Total := @ + Count;
         exit when Count = 0;
      end loop;

      return Total;
   end Number_Of_Recursively_Accessible_Rolls;

   procedure Solve is
   begin
      IO.Put_Line ("Part 1:" & Number_Of_Accessible_Rolls (Floor, False)'Image);
      IO.Put_Line ("Part 2:" & Number_Of_Recursively_Accessible_Rolls (Floor)'Image);
   end Solve;

end Day_04;