with AOC2024_04_Input; use AOC2024_04_Input;

package body AOC2024_04 with SPARK_Mode => Off is

   Input : constant Word_Search := Parse_Input_Data (Input_File_Name);

   function Number_Of_Xmas_Around (Row : Positive; Col : Positive) return Natural;
   -- For an 'X' at (Row, Col), check in each direction whether there is an 'XMAS'.

   function Is_Center_Of_X_Mas (Row : Positive; Col : Positive) return Boolean;
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
         end loop;
      end loop;

      return Number_Of_Occurences;
   end Solution_Part2;

   function Number_Of_Xmas_Around (Row : Positive; Col : Positive) return Natural is
      Count     : Natural := 0;
      Candidate : String (1 .. 4);
   begin
      -- Check to the left (unless it's too close to the left edge).
      if Col >= 4 then
         Candidate := Input (Row, Col) & Input (Row, Col - 1) & Input (Row, Col - 2) & Input (Row, Col - 3);
         if Candidate = "XMAS" then
            Count := @ + 1;
         end if;
      end if;

      -- Check to the right (unless it's too close to the right edge).
      if Col <= 137 then
         Candidate := Input (Row, Col) & Input (Row, Col + 1) & Input (Row, Col + 2) & Input (Row, Col + 3);
         if Candidate = "XMAS" then
            Count := @ + 1;
         end if;
      end if;

      -- Check up (unless it's too close to the top).
      if Row >= 4 then
         Candidate := Input (Row, Col) & Input (Row - 1, Col) & Input (Row - 2, Col) & Input (Row - 3, Col);
         if Candidate = "XMAS" then
            Count := @ + 1;
         end if;
      end if;

      -- Check down (unless it's too close to the bottom).
      if Row <= 137 then
         Candidate := Input (Row, Col) & Input (Row + 1, Col) & Input (Row + 2, Col) & Input (Row + 3, Col);
         if Candidate = "XMAS" then
            Count := @ + 1;
         end if;
      end if;

      -- Check diagonal to the left and up (unless it's too close to the edges).
      if Row >= 4 and Col >= 4 then
         Candidate := Input (Row, Col) & Input (Row - 1, Col - 1) & Input (Row - 2, Col - 2) & Input (Row - 3, Col - 3);
         if Candidate = "XMAS" then
            Count := @ + 1;
         end if;
      end if;

      -- Check diagonal to the right and up (unless it's too close to the edges).
      if Row >= 4 and Col <= 137 then
         Candidate := Input (Row, Col) & Input (Row - 1, Col + 1) & Input (Row - 2, Col + 2) & Input (Row - 3, Col + 3);
         if Candidate = "XMAS" then
            Count := @ + 1;
         end if;
      end if;

      -- Check diagonal to the left and down (unless it's too close to the edges).
      if Row <= 137 and Col >= 4 then
         Candidate := Input (Row, Col) & Input (Row + 1, Col - 1) & Input (Row + 2, Col - 2) & Input (Row + 3, Col - 3);
         if Candidate = "XMAS" then
            Count := @ + 1;
         end if;
      end if;

      -- Check diagonal to the right and down (unless it's too close to the edges).
      if Row <= 137 and Col <= 137 then
         Candidate := Input (Row, Col) & Input (Row + 1, Col + 1) & Input (Row + 2, Col + 2) & Input (Row + 3, Col + 3);
         if Candidate = "XMAS" then
            Count := @ + 1;
         end if;
      end if;

      return Count;
   end Number_Of_Xmas_Around;

   function Is_Center_Of_X_Mas (Row : Positive; Col : Positive) return Boolean is
   begin
      -- If we're too close to an edge, then there is not enough space for an X.
      if Row = 1 or Row = 140 or Col = 1 or Col = 140 then
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