with AOC2024_04_Input; use AOC2024_04_Input;

package body AOC2024_04 with SPARK_Mode => On is

   Input : constant Word_Search := Parse_Input_Data (Input_File_Name);

   function Number_Of_Xmas_Around (Row : Dimension; Col : Dimension) return Natural
      with Post => (Number_Of_Xmas_Around'Result <= 8);
   -- For an 'X' at (Row, Col), check in each direction whether there is an 'XMAS'.

   function Is_Center_Of_X_Mas (Row : Dimension; Col : Dimension) return Boolean;
   -- For an 'A' at (Row, Col), check whether it's the center or a 'MAS' cross.

   function Solution_Part1 return Natural is
      Number_Of_Occurences : Natural := 0;
   begin
      -- Walk over input. For each 'X', check its surroundings
      -- to see whether there is 'XMAS' in any direction.
      for Row in Input'Range (1) loop
         for Col in Input'Range (2) loop
            if Input (Row, Col) = 'X' then
               Number_Of_Occurences := @ + Number_Of_Xmas_Around (Row, Col);
            end if;

            -- In each column of a row, there can be a maximum of 8 matches.
            pragma Loop_Invariant (Number_Of_Occurences <= Natural (Row) * Natural (Col) * 8);
         end loop;
      end loop;

      return Number_Of_Occurences;
   end Solution_Part1;

   function Solution_Part2 return Natural is
      Number_Of_Occurences : Natural := 0;
   begin
      -- Walk over input. For each 'A', check its surroundings
      -- to see whether there is an 'X-MAS' around it.
      for Row in Input'Range (1) loop
         for Col in Input'Range (2) loop
            if Input (Row, Col) = 'A' and Is_Center_Of_X_Mas (Row, Col) then
               Number_Of_Occurences := @ + 1;
            end if;

            -- In each column of a row, there can be a maximum of 1 match.
            pragma Loop_Invariant (Number_Of_Occurences <= Natural (Row) * Natural (Col));
         end loop;
      end loop;

      return Number_Of_Occurences;
   end Solution_Part2;

   function Number_Of_Xmas_Around (Row : Dimension; Col : Dimension) return Natural is
      Count : Natural := 0;
   begin
      -- Check to the left (unless it's too close to the left edge).
      if Col >= Dimension'First + 3 then
         if Input (Row, Col - 1) = 'M' and Input (Row, Col - 2) = 'A' and Input (Row, Col - 3) = 'S' then
            Count := @ + 1;
         end if;
      end if;

      -- Check to the right (unless it's too close to the right edge).
      if Col <= Dimension'Last - 3 then
         if Input (Row, Col + 1) = 'M' and Input (Row, Col + 2) = 'A' and Input (Row, Col + 3) = 'S' then
            Count := @ + 1;
         end if;
      end if;

      -- Check up (unless it's too close to the top).
      if Row >= Dimension'First + 3 then
         if Input (Row - 1, Col) = 'M' and Input (Row - 2, Col) = 'A' and Input (Row - 3, Col) = 'S' then
            Count := @ + 1;
         end if;
      end if;

      -- Check down (unless it's too close to the bottom).
      if Row <= Dimension'Last - 3 then
         if Input (Row + 1, Col) = 'M' and Input (Row + 2, Col) = 'A' and Input (Row + 3, Col) = 'S' then
            Count := @ + 1;
         end if;
      end if;

      -- Check diagonal to the left and up (unless it's too close to the edges).
      if Row >= Dimension'First + 3 and Col >= Dimension'First + 3 then
         if Input (Row - 1, Col - 1) = 'M' and Input (Row - 2, Col - 2) = 'A' and Input (Row - 3, Col - 3) = 'S' then
            Count := @ + 1;
         end if;
      end if;

      -- Check diagonal to the right and up (unless it's too close to the edges).
      if Row >= Dimension'First + 3 and Col <= Dimension'Last - 3 then
         if Input (Row - 1, Col + 1) = 'M' and Input (Row - 2, Col + 2) = 'A' and Input (Row - 3, Col + 3) = 'S' then
            Count := @ + 1;
         end if;
      end if;

      -- Check diagonal to the left and down (unless it's too close to the edges).
      if Row <= Dimension'Last - 3 and Col >= Dimension'First + 3 then
         if Input (Row + 1, Col - 1) = 'M' and Input (Row + 2, Col - 2) = 'A' and Input (Row + 3, Col - 3) = 'S' then
            Count := @ + 1;
         end if;
      end if;

      -- Check diagonal to the right and down (unless it's too close to the edges).
      if Row <= Dimension'Last - 3 and Col <= Dimension'Last - 3 then
         if Input (Row + 1, Col + 1) = 'M' and Input (Row + 2, Col + 2) = 'A' and Input (Row + 3, Col + 3) = 'S' then
            Count := @ + 1;
         end if;
      end if;

      return Count;
   end Number_Of_Xmas_Around;

   function Is_Center_Of_X_Mas (Row : Dimension; Col : Dimension) return Boolean is
   begin
      -- If we're too close to an edge, then there is not enough space for an X.
      if Row = Dimension'First or Row = Dimension'Last or Col = Dimension'First or Col = Dimension'Last then
         return False;
      end if;

      declare
         Char_Upper_Left   : constant Character := Input (Row - 1, Col - 1);
         Char_Upper_Right  : constant Character := Input (Row - 1, Col + 1);
         Char_Lower_Left   : constant Character := Input (Row + 1, Col - 1);
         Char_Lower_Right  : constant Character := Input (Row + 1, Col + 1);
         Diagonal1_Matches : Boolean := False;
         Diagonal2_Matches : Boolean := False;
      begin
         -- Check one diagonal of the X.
         if Char_Upper_Left = 'M' and Char_Lower_Right = 'S' then
            Diagonal1_Matches := True;
         elsif Char_Upper_Left = 'S' and Char_Lower_Right = 'M' then
            Diagonal1_Matches := True;
         end if;

         -- Check the other diagonal of the X.
         if Char_Upper_Right = 'M' and Char_Lower_Left = 'S' then
            Diagonal2_Matches := True;
         elsif Char_Upper_Right = 'S' and Char_Lower_Left = 'M' then
            Diagonal2_Matches := True;
         end if;

         return Diagonal1_Matches and Diagonal2_Matches;
      end;
   end Is_Center_Of_X_Mas;

end AOC2024_04;